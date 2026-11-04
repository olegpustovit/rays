use crate::error;

fn invalid_pattern(message: String) -> error::T {
    error::internal("attribute_pattern", "invalid_pattern", message)
}
#[derive(Clone, PartialEq)]
enum Atom {
    Literal(u8),
    Any,
    Star,
    Class { bits: [u8; 32], negated: bool },
}

struct Term {
    excluded: bool,
    atoms: Vec<Atom>,
}

pub struct T {
    initially_selected: bool,
    terms: Vec<Term>,
}

#[derive(Clone, PartialEq)]
enum CaptureKind {
    Star,
    Any(usize),
    Classes(Vec<Atom>),
}

struct CaptureGroup {
    first_atom: usize,
    atom_count: usize,
    kind: CaptureKind,
}

enum ReplacementAtom {
    Literal(u8),
    Capture(usize),
}

pub struct Rewrite {
    match_atoms: Vec<Atom>,
    capture_of_atom: Vec<i64>,
    capture_groups: Vec<CaptureGroup>,
    replacement_atoms: Vec<ReplacementAtom>,
}

pub struct RewriteSet {
    rules: Vec<Rewrite>,
}

fn whitespace(byte: u8) -> bool {
    matches!(byte, b' ' | b'\t' | b'\n' | b'\r' | 0x0c)
}

fn split_terms(source: &str) -> Result<Vec<Vec<u8>>, String> {
    let source = source.as_bytes();
    let mut terms = Vec::new();
    let mut index = 0;
    while index < source.len() {
        while index < source.len() && whitespace(source[index]) {
            index += 1;
        }
        if index < source.len() {
            let first = index;
            let mut escaped = false;
            while index < source.len() && (escaped || !whitespace(source[index])) {
                let byte = source[index];
                index += 1;
                if escaped {
                    escaped = false;
                } else if byte == b'\\' {
                    escaped = true;
                }
            }
            if escaped {
                return Err("Attribute_pattern: dangling backslash escape".to_owned());
            }
            terms.push(source[first..index].to_vec());
        }
    }
    Ok(terms)
}

fn escaped_character(token: &[u8], index: usize, limit: usize) -> Result<(u8, usize), String> {
    if index >= limit {
        Err("missing class character".to_owned())
    } else if token[index] == b'\\' {
        if index + 1 >= limit {
            Err("dangling class escape".to_owned())
        } else {
            Ok((token[index + 1], index + 2))
        }
    } else {
        Ok((token[index], index + 1))
    }
}

fn class_atom(token: &[u8], first: usize) -> Result<(Atom, usize), String> {
    let mut closing = None;
    let mut index = first;
    let mut escaped = false;
    while index < token.len() && closing.is_none() {
        let byte = token[index];
        if escaped {
            escaped = false;
        } else if byte == b'\\' {
            escaped = true;
        } else if byte == b']' {
            closing = Some(index);
        }
        index += 1;
    }
    let closing = closing.ok_or_else(|| "unterminated character class".to_owned())?;
    if closing == first {
        return Err("empty character class".to_owned());
    }
    let (negated, content_first) = if matches!(token[first], b'!' | b'^') {
        (true, first + 1)
    } else {
        (false, first)
    };
    if content_first == closing {
        return Err("empty negated character class".to_owned());
    }

    let mut bits = [0_u8; 32];
    let mut cursor = content_first;
    while cursor < closing {
        let (left, after_left) = escaped_character(token, cursor, closing)?;
        if after_left < closing && token[after_left] == b'-' && after_left + 1 < closing {
            let (right, after_right) = escaped_character(token, after_left + 1, closing)?;
            if left > right {
                return Err("descending character-class range".to_owned());
            }
            for byte in left..=right {
                bits[(byte >> 3) as usize] |= 1 << (byte & 7);
            }
            cursor = after_right;
        } else {
            bits[(left >> 3) as usize] |= 1 << (left & 7);
            cursor = after_left;
        }
    }
    Ok((Atom::Class { bits, negated }, closing + 1))
}

fn compile_term(raw: &[u8]) -> Result<Term, String> {
    let (excluded, first) = if raw.first() == Some(&b'^') {
        (true, 1)
    } else {
        (false, 0)
    };
    if first == raw.len() {
        return Err("empty exclusion term".to_owned());
    }
    let mut atoms = Vec::new();
    let mut index = first;
    while index < raw.len() {
        match raw[index] {
            b'\\' => {
                if index + 1 == raw.len() {
                    return Err("dangling backslash escape".to_owned());
                }
                atoms.push(Atom::Literal(raw[index + 1]));
                index += 2;
            }
            b'*' => {
                if !matches!(atoms.last(), Some(Atom::Star)) {
                    atoms.push(Atom::Star);
                }
                index += 1;
            }
            b'?' => {
                atoms.push(Atom::Any);
                index += 1;
            }
            b'[' => {
                let (atom, next) = class_atom(raw, index + 1)?;
                atoms.push(atom);
                index = next;
            }
            byte => {
                atoms.push(Atom::Literal(byte));
                index += 1;
            }
        }
    }
    Ok(Term { excluded, atoms })
}

pub fn compile(source: &str) -> Result<T, error::T> {
    let raw_terms = split_terms(source).map_err(invalid_pattern)?;
    let mut terms = Vec::with_capacity(raw_terms.len());
    for (index, raw) in raw_terms.iter().enumerate() {
        terms.push(compile_term(raw).map_err(|message| {
            invalid_pattern(format!("Attribute_pattern: term {}: {message}", index + 1))
        })?);
    }
    let initially_selected = terms.is_empty() || terms[0].excluded;
    Ok(T { initially_selected, terms })
}

fn atom_matches(atom: &Atom, byte: u8) -> bool {
    match atom {
        Atom::Literal(expected) => byte == *expected,
        Atom::Any => true,
        Atom::Class { bits, negated } => {
            (bits[(byte >> 3) as usize] & (1 << (byte & 7)) != 0) != *negated
        }
        Atom::Star => false,
    }
}

fn trailing_stars(atoms: &[Atom], mut atom: usize) -> bool {
    while atom < atoms.len() {
        if !matches!(&atoms[atom], Atom::Star) {
            return false;
        }
        atom += 1;
    }
    true
}

fn match_atoms(atoms: &[Atom], name: &[u8]) -> bool {
    let mut atom = 0;
    let mut character = 0;
    let mut last_star = None;
    while character < name.len() {
        if atom < atoms.len() {
            match &atoms[atom] {
                Atom::Star => {
                    last_star = Some((atom, character));
                    atom += 1;
                    continue;
                }
                candidate if atom_matches(candidate, name[character]) => {
                    atom += 1;
                    character += 1;
                    continue;
                }
                _ => {}
            }
        }
        match last_star {
            Some((star, retry)) if retry < name.len() => {
                let retry = retry + 1;
                last_star = Some((star, retry));
                atom = star + 1;
                character = retry;
            }
            _ => return false,
        }
    }
    trailing_stars(atoms, atom)
}

fn evaluate(terms: &[Term], name: &[u8], mut selected: bool) -> bool {
    for term in terms {
        if match_atoms(&term.atoms, name) {
            selected = !term.excluded;
        }
    }
    selected
}

pub fn apply(selected: bool, value: &T, name: &str) -> bool {
    evaluate(&value.terms, name.as_bytes(), selected)
}

pub fn matches(value: &T, name: &str) -> bool {
    apply(value.initially_selected, value, name)
}

fn capture_groups(atoms: &[Atom]) -> (Vec<CaptureGroup>, Vec<i64>) {
    let mut capture_of_atom = vec![-1; atoms.len()];
    let mut groups = Vec::new();
    let mut atom = 0;
    while atom < atoms.len() {
        let first_atom = atom;
        let (kind, count) = match &atoms[atom] {
            Atom::Literal(_) => {
                atom += 1;
                continue;
            }
            Atom::Star => (CaptureKind::Star, 1),
            Atom::Any => {
                while atom < atoms.len() && matches!(&atoms[atom], Atom::Any) {
                    atom += 1;
                }
                (CaptureKind::Any(atom - first_atom), atom - first_atom)
            }
            Atom::Class { .. } => {
                while atom < atoms.len() && matches!(&atoms[atom], Atom::Class { .. }) {
                    atom += 1;
                }
                let count = atom - first_atom;
                (CaptureKind::Classes(atoms[first_atom..atom].to_vec()), count)
            }
        };
        if matches!(&kind, CaptureKind::Star) {
            atom += 1;
        }
        let group_index = groups.len() as i64;
        for index in first_atom..first_atom + count {
            capture_of_atom[index] = group_index;
        }
        groups.push(CaptureGroup { first_atom, atom_count: count, kind });
    }
    (groups, capture_of_atom)
}

fn one_term(label: &str, source: &str) -> Result<Vec<Atom>, String> {
    let terms = split_terms(source)?;
    if terms.len() != 1 {
        return Err(format!("Attribute_pattern: {label} must contain exactly one glob term"));
    }
    let term = compile_term(&terms[0])?;
    if term.excluded {
        return Err(format!("Attribute_pattern: {label} cannot be an exclusion"));
    }
    Ok(term.atoms)
}

pub fn compile_rewrite(pattern: &str, replacement: &str) -> Result<Rewrite, error::T> {
    let match_atoms = one_term("rewrite pattern", pattern).map_err(invalid_pattern)?;
    let replacement_source = one_term("rewrite replacement", replacement).map_err(invalid_pattern)?;
    let (capture_groups, capture_of_atom) = self::capture_groups(&match_atoms);
    let (replacement_groups, replacement_of_atom) = self::capture_groups(&replacement_source);
    if capture_groups.len() != replacement_groups.len() {
        return Err(invalid_pattern(format!(
            "Attribute_pattern: rewrite wildcard count differs ({} source, {} replacement)",
            capture_groups.len(),
            replacement_groups.len()
        )));
    }

    let mut used = vec![false; capture_groups.len()];
    let mut replacement_capture = vec![0; replacement_groups.len()];
    for (replacement_index, replacement_group) in replacement_groups.iter().enumerate() {
        let matching = capture_groups.iter().enumerate().find_map(|(index, group)| {
            (!used[index] && group.kind == replacement_group.kind).then_some(index)
        });
        let found = matching.or_else(|| used.iter().position(|value| !value));
        let Some(found) = found else {
            return Err(invalid_pattern("Attribute_pattern: rewrite wildcard mapping failed".to_owned()));
        };
        used[found] = true;
        replacement_capture[replacement_index] = found;
    }

    let mut replacement_atoms = Vec::new();
    let mut atom = 0;
    while atom < replacement_source.len() {
        match &replacement_source[atom] {
            Atom::Literal(byte) => {
                replacement_atoms.push(ReplacementAtom::Literal(*byte));
                atom += 1;
            }
            Atom::Any | Atom::Star | Atom::Class { .. } => {
                let group = replacement_of_atom[atom] as usize;
                replacement_atoms.push(ReplacementAtom::Capture(replacement_capture[group]));
                atom = replacement_groups[group].first_atom + replacement_groups[group].atom_count;
            }
        }
    }
    Ok(Rewrite { match_atoms, capture_of_atom, capture_groups, replacement_atoms })
}

fn capture_match(rewrite: &Rewrite, name: &[u8]) -> Option<(Vec<usize>, Vec<usize>)> {
    let atoms = &rewrite.match_atoms;
    let mut starts = vec![0; rewrite.capture_groups.len()];
    let mut lengths = vec![0; rewrite.capture_groups.len()];
    let mut atom = 0;
    let mut character = 0;
    let mut last_star = None;

    while character < name.len() {
        if atom < atoms.len() {
            match &atoms[atom] {
                Atom::Star => {
                    let capture = rewrite.capture_of_atom[atom] as usize;
                    starts[capture] = character;
                    lengths[capture] = 0;
                    last_star = Some((atom, character));
                    atom += 1;
                    continue;
                }
                candidate if atom_matches(candidate, name[character]) => {
                    let capture = rewrite.capture_of_atom[atom];
                    if capture >= 0 {
                        let capture = capture as usize;
                        let group = &rewrite.capture_groups[capture];
                        if atom == group.first_atom {
                            starts[capture] = character;
                        }
                        lengths[capture] = character - starts[capture] + 1;
                    }
                    atom += 1;
                    character += 1;
                    continue;
                }
                _ => {}
            }
        }
        match last_star {
            Some((star, retry)) if retry < name.len() => {
                let retry = retry + 1;
                let capture = rewrite.capture_of_atom[star] as usize;
                lengths[capture] = retry - starts[capture];
                last_star = Some((star, retry));
                atom = star + 1;
                character = retry;
            }
            _ => return None,
        }
    }
    while atom < atoms.len() {
        if !matches!(&atoms[atom], Atom::Star) {
            return None;
        }
        let capture = rewrite.capture_of_atom[atom] as usize;
        starts[capture] = character;
        lengths[capture] = 0;
        atom += 1;
    }
    Some((starts, lengths))
}

pub fn rewrite(rule: &Rewrite, name: &str) -> Option<Vec<u8>> {
    let bytes = name.as_bytes();
    let (starts, lengths) = capture_match(rule, bytes)?;
    let mut output = Vec::with_capacity(bytes.len() + 16);
    for atom in &rule.replacement_atoms {
        match atom {
            ReplacementAtom::Literal(byte) => output.push(*byte),
            ReplacementAtom::Capture(capture) => {
                output.extend_from_slice(&bytes[starts[*capture]..starts[*capture] + lengths[*capture]]);
            }
        }
    }
    Some(output)
}

pub fn compile_rewrite_set(pattern: &str, replacement: &str) -> Result<RewriteSet, error::T> {
    let source_terms = split_terms(pattern).map_err(invalid_pattern)?;
    let replacement_terms = split_terms(replacement).map_err(invalid_pattern)?;
    let mut positive = Vec::new();
    for (index, raw) in source_terms.iter().enumerate() {
        let term = compile_term(raw).map_err(|message| {
            invalid_pattern(format!("Attribute_pattern: rewrite source term {}: {message}", index + 1))
        })?;
        if !term.excluded {
            positive.push(raw.as_slice());
        }
    }
    if positive.is_empty() {
        return Err(invalid_pattern("Attribute_pattern: rewrite pattern has no positive terms".to_owned()));
    }
    if positive.len() != replacement_terms.len() {
        return Err(invalid_pattern(format!(
            "Attribute_pattern: rewrite term count differs ({} positive source, {} replacement)",
            positive.len(),
            replacement_terms.len()
        )));
    }
    let mut rules = Vec::with_capacity(positive.len());
    for (source, replacement) in positive.iter().zip(replacement_terms.iter()) {
        let source = std::str::from_utf8(source).map_err(|_| {
            invalid_pattern("Attribute_pattern: rewrite pattern is not valid UTF-8".to_owned())
        })?;
        let replacement = std::str::from_utf8(replacement).map_err(|_| {
            invalid_pattern("Attribute_pattern: rewrite replacement is not valid UTF-8".to_owned())
        })?;
        rules.push(compile_rewrite(source, replacement)?);
    }
    Ok(RewriteSet { rules })
}

pub fn rewrite_set(rules: &RewriteSet, name: &str) -> Option<Vec<u8>> {
    let mut output = None;
    for rule in &rules.rules {
        if let Some(value) = rewrite(rule, name) {
            output = Some(value);
        }
    }
    output
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn implicit_selection_and_glob_terms() {
        let pattern = compile("^bar score").unwrap();
        assert!(!apply(true, &pattern, "bar"));
        assert!(apply(false, &pattern, "score"));
        assert!(matches(&pattern, "score"));
        assert!(matches(&pattern, "other"));
    }

    #[test]
    fn byte_classes_and_rewrite_capture_shapes() {
        let pattern = compile("item_[!0-9]?").unwrap();
        assert!(matches(&pattern, "item_aZ"));
        assert!(!matches(&pattern, "item_4Z"));

        let rule = compile_rewrite("*_???", "old_???_*").unwrap();
        assert_eq!(rewrite(&rule, "tail_abc").as_deref(), Some(&b"old_abc_tail"[..]));
    }

    #[test]
    fn rewrite_set_uses_last_matching_positive_rule() {
        let rules =
            compile_rewrite_set("point_* point_a*", "renamed_* special_*").unwrap();
        assert_eq!(rewrite_set(&rules, "point_ab").as_deref(), Some(&b"special_b"[..]));
        assert_eq!(rewrite_set(&rules, "normal"), None);
    }
    #[test]
    fn rewrite_preserves_reordered_utf8_bytes() {
        let rule = compile_rewrite("?*", "*?").unwrap();
        assert_eq!(rewrite(&rule, "é"), Some(vec![0xa9, 0xc3]));
    }
}
