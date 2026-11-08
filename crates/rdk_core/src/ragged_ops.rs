use std::mem::take;

use crate::{cancel, error, packed, support};
use crate::error::Code;

const DEFAULT_GRAIN: i64 = 16_384;

fn invalid_ragged(message: String) -> error::T {
    error::new("ragged_ops", Code::InvalidRagged, message)
}

fn cancelled_error() -> error::T {
    error::cancelled("ragged_ops")
}
fn run_indexed_ranges(
    grain: i64,
    count: i64,
    work: &(dyn Fn(i64, i64, i64) -> Result<(), error::T> + Sync),
) -> Result<(), error::T> {
    support::run_indexed_ranges(grain, count, work)
}

fn validate_source_index(operation: &str, source_count: i64, source: i64) -> Result<(), error::T> {
    if source < -1 || source >= source_count {
        Err(invalid_ragged(format!("{operation}: source mapping index is out of bounds")))
    } else {
        Ok(())
    }
}

fn offsets_for_mapping(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    operation: &str,
    source_offsets: &[i64],
    mapping: &[i64],
) -> Result<Vec<i64>, error::T> {
    if grain <= 0 {
        return Err(invalid_ragged(format!("{operation}: grain must be positive")));
    }
    let source_count = i64::try_from(source_offsets.len())
        .map_err(|_| invalid_ragged(format!("{operation}: source row count overflow")))?
        - 1;
    for (output, &source) in mapping.iter().enumerate() {
        cancel::poll(cancel_token, output as i64, 4095).map_err(|_| cancelled_error())?;
        validate_source_index(operation, source_count, source)?;
    }

    let mut offsets = vec![0_i64; mapping.len().checked_add(1)
        .ok_or_else(|| invalid_ragged(format!("{operation}: output row count overflow")))?];
    support::par_fill(cancel_token, grain, &mut offsets[1..], &|output| {
        let source = mapping[output as usize];
        Ok(if source >= 0 {
            let source = source as usize;
            source_offsets[source + 1] - source_offsets[source]
        } else {
            0
        })
    })?;
    let mut total = 0_i64;
    for (output, offset) in offsets[1..].iter_mut().enumerate() {
        cancel::poll(cancel_token, output as i64, 4095).map_err(|_| cancelled_error())?;
        if *offset > i64::MAX - total {
            return Err(invalid_ragged(format!("{operation}: packed value count overflow")));
        }
        total += *offset;
        *offset = total;
    }
    Ok(offsets)
}

fn remapped_values<T: Copy + Default + Send + Sync>(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    mapping: &[i64],
    source_offsets: &[i64],
    source_values: &[T],
    offsets: &[i64],
    operation: &str,
) -> Result<Vec<T>, error::T> {
    let count = i64::try_from(mapping.len())
        .map_err(|_| invalid_ragged(format!("{operation}: output row count overflow")))?;
    let total = usize::try_from(offsets[offsets.len() - 1])
        .map_err(|_| invalid_ragged(format!("{operation}: packed value count overflow")))?;
    let mut values = vec![T::default(); total];
    let windows = support::Windows::new(&mut values, support::range_count(count, grain), |range| {
        offsets[range.saturating_mul(grain).min(count) as usize] as usize
    });
    run_indexed_ranges(grain, count, &|range, first, last| {
        let mut window = windows.take(range);
        for output in first..last {
            cancel::poll(cancel_token, output, 4095).map_err(|_| cancelled_error())?;
            let source = mapping[output as usize];
            if source >= 0 {
                let source = source as usize;
                let row = &source_values[source_offsets[source] as usize..source_offsets[source + 1] as usize];
                let (head, tail) = take(&mut window).split_at_mut(row.len());
                head.copy_from_slice(row);
                window = tail;
            }
        }
        Ok(())
    })?;
    Ok(values)
}

pub fn remap_int(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    mapping: &[i64],
    source: &packed::IntArray::T,
) -> Result<packed::IntArray::T, error::T> {
    let grain = grain.unwrap_or(DEFAULT_GRAIN);
    let view = packed::IntArray::View::view(source);
    let operation = "Ragged_ops.remap_int";
    let offsets = offsets_for_mapping(cancel_token, grain, operation, &view.offsets, mapping)?;
    let values = remapped_values(cancel_token, grain, mapping, &view.offsets, &view.values, &offsets, operation)?;
    Ok(packed::IntArray::View::create_validated_owned(offsets, values))
}

pub fn remap_float(
    cancel_token: Option<&cancel::T>,
    grain: Option<i64>,
    mapping: &[i64],
    source: &packed::FloatArray::T,
) -> Result<packed::FloatArray::T, error::T> {
    let grain = grain.unwrap_or(DEFAULT_GRAIN);
    let view = packed::FloatArray::View::view(source);
    let operation = "Ragged_ops.remap_float";
    let offsets = offsets_for_mapping(cancel_token, grain, operation, &view.offsets, mapping)?;
    let values = remapped_values(cancel_token, grain, mapping, &view.offsets, &view.values, &offsets, operation)?;
    Ok(packed::FloatArray::View::create_validated_owned(offsets, values))
}

fn concat_layout_int(views: &[packed::IntArray::View::View]) -> Result<(i64, i64), error::T> {
    let mut total_rows = 0_i64;
    let mut total_values = 0_i64;
    for view in views {
        let rows = i64::try_from(view.offsets.len()).unwrap_or(i64::MAX) - 1;
        let values = i64::try_from(view.values.len()).unwrap_or(i64::MAX);
        if rows > i64::MAX - total_rows || values > i64::MAX - total_values {
            return Err(invalid_ragged("Ragged_ops.concat_int: packed cardinality overflow".to_owned()));
        }
        total_rows += rows;
        total_values += values;
    }
    if total_rows == i64::MAX {
        return Err(invalid_ragged("Ragged_ops.concat_int: packed cardinality overflow".to_owned()));
    }
    Ok((total_rows, total_values))
}

fn concat_layout_float(views: &[packed::FloatArray::View::View]) -> Result<(i64, i64), error::T> {
    let mut total_rows = 0_i64;
    let mut total_values = 0_i64;
    for view in views {
        let rows = i64::try_from(view.offsets.len()).unwrap_or(i64::MAX) - 1;
        let values = i64::try_from(view.values.len()).unwrap_or(i64::MAX);
        if rows > i64::MAX - total_rows || values > i64::MAX - total_values {
            return Err(invalid_ragged("Ragged_ops.concat_float: packed cardinality overflow".to_owned()));
        }
        total_rows += rows;
        total_values += values;
    }
    if total_rows == i64::MAX {
        return Err(invalid_ragged("Ragged_ops.concat_float: packed cardinality overflow".to_owned()));
    }
    Ok((total_rows, total_values))
}

pub fn concat_int(sources: &[packed::IntArray::T]) -> Result<packed::IntArray::T, error::T> {
    let views: Vec<packed::IntArray::View::View> = sources.iter().map(packed::IntArray::View::view).collect();
    let (rows, value_count) = concat_layout_int(&views)?;
    let rows = usize::try_from(rows)
        .map_err(|_| invalid_ragged("Ragged_ops.concat_int: packed cardinality overflow".to_owned()))?;
    let value_count = usize::try_from(value_count)
        .map_err(|_| invalid_ragged("Ragged_ops.concat_int: packed cardinality overflow".to_owned()))?;
    let mut offsets = Vec::with_capacity(rows + 1);
    let mut values = Vec::with_capacity(value_count);
    offsets.push(0);
    for view in &views {
        for row in 0..view.offsets.len() - 1 {
            let first = view.offsets[row] as usize;
            let last = view.offsets[row + 1] as usize;
            values.extend_from_slice(&view.values[first..last]);
            offsets.push(values.len() as i64);
        }
    }
    Ok(packed::IntArray::View::create_validated_owned(offsets, values))
}

pub fn concat_float(sources: &[packed::FloatArray::T]) -> Result<packed::FloatArray::T, error::T> {
    let views: Vec<packed::FloatArray::View::View> = sources.iter().map(packed::FloatArray::View::view).collect();
    let (rows, value_count) = concat_layout_float(&views)?;
    let rows = usize::try_from(rows)
        .map_err(|_| invalid_ragged("Ragged_ops.concat_float: packed cardinality overflow".to_owned()))?;
    let value_count = usize::try_from(value_count)
        .map_err(|_| invalid_ragged("Ragged_ops.concat_float: packed cardinality overflow".to_owned()))?;
    let mut offsets = Vec::with_capacity(rows + 1);
    let mut values = Vec::with_capacity(value_count);
    offsets.push(0);
    for view in &views {
        for row in 0..view.offsets.len() - 1 {
            let first = view.offsets[row] as usize;
            let last = view.offsets[row + 1] as usize;
            values.extend_from_slice(&view.values[first..last]);
            offsets.push(values.len() as i64);
        }
    }
    Ok(packed::FloatArray::View::create_validated_owned(offsets, values))
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn remap_and_concat_preserve_empty_rows_and_order() {
        let source = packed::IntArray::create_owned(vec![0, 2, 2, 3], vec![4, 5, 7]).unwrap();
        let remapped = remap_int(None, Some(1), &[2, -1, 0, 2], &source).unwrap();
        let view = packed::IntArray::View::view(&remapped);
        assert_eq!(&*view.offsets, &[0, 1, 1, 3, 4]);
        assert_eq!(&*view.values, &[7, 4, 5, 7]);

        let empty = packed::IntArray::create_owned(vec![0], Vec::new()).unwrap();
        let concatenated = concat_int(&[source, empty]).unwrap();
        let view = packed::IntArray::View::view(&concatenated);
        assert_eq!(&*view.offsets, &[0, 2, 2, 3]);
        assert_eq!(&*view.values, &[4, 5, 7]);
    }

    #[test]
    fn float_rows_concatenate_in_source_order() {
        let first = packed::FloatArray::create_owned(vec![0, 1], vec![1.5]).unwrap();
        let second = packed::FloatArray::create_owned(vec![0, 2], vec![2.5, 3.5]).unwrap();
        let concatenated = concat_float(&[first, second]).unwrap();
        let view = packed::FloatArray::View::view(&concatenated);
        assert_eq!(&*view.offsets, &[0, 1, 3]);
        assert_eq!(&*view.values, &[1.5, 2.5, 3.5]);
    }
    #[test]
    fn remap_matches_across_domains_with_many_ranges() {
        let source = packed::IntArray::create_owned(vec![0, 2, 2, 3, 6], vec![4, 5, 7, 8, 9, 10]).unwrap();
        let mapping: Vec<i64> = (0..100).map(|i| (i % 5) - 1).collect();
        let run = |domains| {
            let slot = std::sync::Mutex::new(None);
            parallel::run_unit(Some(domains), &|| {
                let out = remap_int(None, Some(3), &mapping, &source).unwrap();
                let view = packed::IntArray::View::view(&out);
                *slot.lock().unwrap() = Some((view.offsets.to_vec(), view.values.to_vec()));
                Ok(())
            })
            .unwrap();
            slot.into_inner().unwrap().unwrap()
        };
        let (offsets, values) = run(1);
        assert_eq!(offsets.len(), 101);
        assert_eq!(values.len(), offsets[100] as usize);
        assert_eq!((offsets, values), run(4));
    }
    #[test]
    fn invalid_mapping_is_not_reported_as_cancellation() {
        let source = packed::IntArray::create_owned(vec![0, 1], vec![7]).unwrap();
        let error = remap_int(None, Some(1), &[-2], &source).err().unwrap();
        assert_eq!(error::code(&error), Code::InvalidRagged);
    }
    #[test]
    fn range_worker_errors_are_preserved() {
        let worker_error = error::new("worker", Code::WorkerFailure, "range failure".to_owned());
        let result = run_indexed_ranges(1, 1, &|_, _, _| Err(worker_error.clone()));
        let error = result.unwrap_err();
        assert_eq!(error::code(&error), Code::WorkerFailure);
        assert_eq!(error::message(&error), "range failure");
    }
}
