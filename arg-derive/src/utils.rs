pub fn to_hyphenated_lower_case(input: &str) -> String {
    let mut output = String::with_capacity(input.len());

    let mut chars = input.chars();
    let mut prev_upper = false;
    if let Some(ch) = chars.next() {
        if ch.is_uppercase() {
            prev_upper = true;
            for ch in ch.to_lowercase() {
                output.push(ch);
            }
        } else {
            output.push(ch);
        }
    }

    for ch in chars {
        if ch.is_uppercase() {
            if !prev_upper {
                output.push('-');
            }

            for ch in ch.to_lowercase() {
                output.push(ch);
            }

            prev_upper = true;
        } else {
            output.push(ch);

            prev_upper = false;
        }
    }

    output
}
