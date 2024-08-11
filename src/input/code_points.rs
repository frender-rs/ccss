// U+005C REVERSE SOLIDUS
pub const REVERSE_SOLIDUS: char = '\u{005C}';

pub const LF: char = '\u{000A}';

// U+0028 LEFT PARENTHESIS (()
pub const LEFT_PARENTHESIS: char = '\u{0028}';
// U+0029 RIGHT PARENTHESIS ())
pub const RIGHT_PARENTHESIS: char = '\u{0029}';

pub const HYPHEN_MINUS: char = '\u{002D}';

pub const PERCENTAGE_SIGN: char = '\u{0025}';

// U+000D CARRIAGE RETURN (CR)
pub(crate) const CR: char = '\u{000D}';
// U+000C FORM FEED (FF)
pub(crate) const FF: char = '\u{000C}';

/// https://drafts.csswg.org/css-syntax-3/#ident-start-code-point
pub const fn is_ident_start_code_point(c: char) -> bool {
    match c {
        // letter
        '\u{0041}'..='\u{005A}' | '\u{0061}'..='\u{007A}' => true,
        // non-ASCII ident code point
        '\u{00B7}'
        | '\u{00C0}'..='\u{00D6}'
        | '\u{00D8}'..='\u{00F6}'
        | '\u{00F8}'..='\u{037D}'
        | '\u{037F}'..='\u{1FFF}'
        | '\u{200C}'
        | '\u{200D}'
        | '\u{203F}'
        | '\u{2040}'
        | '\u{2070}'..='\u{218F}'
        | '\u{2C00}'..='\u{2FEF}'
        | '\u{3001}'..='\u{D7FF}'
        | '\u{F900}'..='\u{FDCF}'
        | '\u{FDF0}'..='\u{FFFD}'
        | '\u{10000}'.. => true,
        // U+005F LOW LINE (_)
        '\u{005F}' => true,
        _ => false,
    }
}

/// https://drafts.csswg.org/css-syntax-3/#ident-code-point
pub const fn is_ident_code_point(c: char) -> bool {
    match c {
        // digit
        '\u{0030}'..='\u{0039}' => true,
        // U+002D HYPHEN-MINUS (-)
        '\u{002D}' => true,
        c => is_ident_start_code_point(c),
    }
}
