use oxymp::MyTrait;

trait MyTrait {
    fn answer() -> i32 {
        42
    }
}

#[derive(MyTrait)]
struct Foo;

fn main() {
    println!("{}", Foo::answer());
}
