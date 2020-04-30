//! # Art
//!
//! A library for modeling artistic concepts.

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

pub use self::kinds::PrimaryColor;
pub use self::kinds::SecondaryColor;
pub use self::utils::mix;

///kinds mod
pub mod kinds {
    /// The primary colors according to the RYB color model
    pub enum PrimaryColor {
        Red,
        Yellow,
        Blue,
    }

    /// The secondary colors according to the RYB color model.
    pub enum SecondaryColor {
        Orange,
        Green,
        Purple,
    }

    /// Print a primary color
    pub fn print_primary_color(color: &PrimaryColor) {
        match color {
            PrimaryColor::Red => println!("Primary color is red!"),
            PrimaryColor::Yellow => println!("Primary color is yellow!"),
            PrimaryColor::Blue => println!("Primary color is blue!"),
        }
    }
}

pub mod utils {
    use crate::kinds::*;

    /// Combines two primary colors in equal amounts to create
    /// a secondary color.
    pub fn mix(c1: PrimaryColor, c2: PrimaryColor) -> SecondaryColor {
        print_primary_color(&c1);
        print_primary_color(&c2);
        SecondaryColor::Orange
    }
}
