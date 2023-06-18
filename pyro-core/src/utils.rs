pub fn generate_generic_type_name(mut n: usize) -> String {
    let mut result = String::new();
    loop {
        let ch = ((n % 26) as u8 + b'a') as char;
        result.push(ch);
        n /= 26;
        if n > 0 {
            n -= 1;
        } else {
            break;
        }
    }

    result.chars().rev().collect::<String>()
}
