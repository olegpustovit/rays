// Atlas fixture 36: stable scopes, Unicode λ and a literal \ path.
pub mod layer_36 {
    pub struct Tile { pub width: f32 }
    impl Tile {
        pub fn area(&self) -> f32 { self.width * self.width }
    }
    pub fn sample(x: i32) -> i32 {
        fn nested(y: i32) -> i32 { y + 1 }
        nested(x) + 36
    }
}
