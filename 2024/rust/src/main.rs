// Fuck this language. I'll try again in another five years.
// C has better ergonomics than this trash.
mod day1;
mod day2;
mod common;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        println!("Please provide a day number");
        return;
    } else {
        let day = &args[1];
        if args.len() < 3 {
            println!("Please provide an input file");
            return;
        }
        let result = match day.as_str() {
            "1" => day1::run(&args[2]),
            "2" => day2::run(&args[2]),
            _ => {
                println!("Day {} not implemented", day);
                return;
            }
        };
        match result {
            Ok(_) => (),
            Err(e) => println!("Error: {}", e),
        }
    }
}
