use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Mutex;

use crate::{cancel, error, packed, support};

const DEFAULT_GRAIN: i64 = 16_384;

fn invalid_ragged(message: String) -> error::T {
    error::internal("ragged_ops", "invalid_ragged", message)
}

fn cancelled_error() -> error::T {
    error::internal("ragged_ops", "cancelled", "ragged operation was cancelled".to_owned())
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
    let output_count = i64::try_from(mapping.len())
        .map_err(|_| invalid_ragged(format!("{operation}: output row count overflow")))?;
    let source_count = i64::try_from(source_offsets.len())
        .map_err(|_| invalid_ragged(format!("{operation}: source row count overflow")))?
        - 1;
    for (output, &source) in mapping.iter().enumerate() {
        cancel::poll(cancel_token, output as i64, 4095).map_err(|_| cancelled_error())?;
        validate_source_index(operation, source_count, source)?;
    }

    let lengths: Vec<AtomicI64> = (0..mapping.len()).map(|_| AtomicI64::new(0)).collect();
    support::parallel_for(cancel_token, grain, output_count, &|output| {
        let source = mapping[output as usize];
        if source >= 0 {
            let source = source as usize;
            lengths[output as usize].store(source_offsets[source + 1] - source_offsets[source], Ordering::Relaxed);
        }
        Ok(())
    })?;

    let offset_capacity = mapping.len().checked_add(1)
        .ok_or_else(|| invalid_ragged(format!("{operation}: output row count overflow")))?;
    let mut offsets = Vec::with_capacity(offset_capacity);
    let mut total = 0_i64;
    offsets.push(total);
    for output in 0..mapping.len() {
        cancel::poll(cancel_token, output as i64, 4095).map_err(|_| cancelled_error())?;
        let length = lengths[output].load(Ordering::Relaxed);
        if length > i64::MAX - total {
            return Err(invalid_ragged(format!("{operation}: packed value count overflow")));
        }
        total += length;
        offsets.push(total);
    }
    Ok(offsets)
}

fn remapped_int_values(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    mapping: &[i64],
    source: &packed::IntArray::View::View,
    offsets: &[i64],
    operation: &str,
) -> Result<Vec<i64>, error::T> {
    remapped_values_int(cancel_token, grain, mapping, &source.offsets, &source.values, offsets, operation)
}

fn remapped_float_values(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    mapping: &[i64],
    source: &packed::FloatArray::View::View,
    offsets: &[i64],
    operation: &str,
) -> Result<Vec<f64>, error::T> {
    remapped_values_float(cancel_token, grain, mapping, &source.offsets, &source.values, offsets, operation)
}

fn range_count(count: i64, grain: i64) -> usize {
    if count <= 0 { 0 } else { ((count - 1) / grain + 1) as usize }
}

fn remapped_values_int(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    mapping: &[i64],
    source_offsets: &[i64],
    source_values: &[i64],
    offsets: &[i64],
    operation: &str,
) -> Result<Vec<i64>, error::T> {
    let count = i64::try_from(mapping.len())
        .map_err(|_| invalid_ragged(format!("{operation}: output row count overflow")))?;
    let ranges = range_count(count, grain);
    let slots: Vec<Mutex<Option<Vec<i64>>>> = (0..ranges).map(|_| Mutex::new(None)).collect();
    run_indexed_ranges(grain, count, &|range, first, last| {
        let capacity = (offsets[last as usize] - offsets[first as usize]) as usize;
        let mut local = Vec::with_capacity(capacity);
        for output in first..last {
            cancel::poll(cancel_token, output, 4095).map_err(|_| cancelled_error())?;
            let source = mapping[output as usize];
            if source >= 0 {
                let source = source as usize;
                local.extend_from_slice(&source_values[source_offsets[source] as usize..source_offsets[source + 1] as usize]);
            }
        }
        *slots[range as usize].lock().unwrap_or_else(|poisoned| poisoned.into_inner()) = Some(local);
        Ok(())
    })?;
    let total = usize::try_from(offsets[offsets.len() - 1])
        .map_err(|_| invalid_ragged(format!("{operation}: packed value count overflow")))?;
    let mut values = Vec::with_capacity(total);
    for slot in slots {
        let range = slot.into_inner().unwrap_or_else(|poisoned| poisoned.into_inner());
        let Some(range) = range else {
            return Err(invalid_ragged(format!("{operation}: missing output range")));
        };
        values.extend(range);
    }
    Ok(values)
}

fn remapped_values_float(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    mapping: &[i64],
    source_offsets: &[i64],
    source_values: &[f64],
    offsets: &[i64],
    operation: &str,
) -> Result<Vec<f64>, error::T> {
    let count = i64::try_from(mapping.len())
        .map_err(|_| invalid_ragged(format!("{operation}: output row count overflow")))?;
    let ranges = range_count(count, grain);
    let slots: Vec<Mutex<Option<Vec<f64>>>> = (0..ranges).map(|_| Mutex::new(None)).collect();
    run_indexed_ranges(grain, count, &|range, first, last| {
        let capacity = (offsets[last as usize] - offsets[first as usize]) as usize;
        let mut local = Vec::with_capacity(capacity);
        for output in first..last {
            cancel::poll(cancel_token, output, 4095).map_err(|_| cancelled_error())?;
            let source = mapping[output as usize];
            if source >= 0 {
                let source = source as usize;
                local.extend_from_slice(&source_values[source_offsets[source] as usize..source_offsets[source + 1] as usize]);
            }
        }
        *slots[range as usize].lock().unwrap_or_else(|poisoned| poisoned.into_inner()) = Some(local);
        Ok(())
    })?;
    let total = usize::try_from(offsets[offsets.len() - 1])
        .map_err(|_| invalid_ragged(format!("{operation}: packed value count overflow")))?;
    let mut values = Vec::with_capacity(total);
    for slot in slots {
        let range = slot.into_inner().unwrap_or_else(|poisoned| poisoned.into_inner());
        let Some(range) = range else {
            return Err(invalid_ragged(format!("{operation}: missing output range")));
        };
        values.extend(range);
    }
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
    let values = remapped_int_values(cancel_token, grain, mapping, &view, &offsets, operation)?;
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
    let values = remapped_float_values(cancel_token, grain, mapping, &view, &offsets, operation)?;
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
    fn invalid_mapping_is_not_reported_as_cancellation() {
        let source = packed::IntArray::create_owned(vec![0, 1], vec![7]).unwrap();
        let error = remap_int(None, Some(1), &[-2], &source).err().unwrap();
        assert_eq!(crate::error::code(&error), "invalid_ragged");
    }
    #[test]
    fn range_worker_errors_are_preserved() {
        let worker_error = error::internal("worker", "worker_failure", "range failure".to_owned());
        let result = run_indexed_ranges(1, 1, &|_, _, _| Err(worker_error.clone()));
        let error = result.unwrap_err();
        assert_eq!(error::code(&error), "worker_failure");
        assert_eq!(error::message(&error), "range failure");
    }
}
