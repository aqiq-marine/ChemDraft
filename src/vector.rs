use std::ops;
use std::f64::consts::PI;

#[derive(Debug, Default, Clone)]
pub struct Vector {
    pub x: f64,
    pub y: f64,
}

impl ops::Add<&Vector> for &Vector {
    type Output = Vector;

    fn add(self, rhs: &Vector) -> Vector {
        Vector {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}
impl ops::Add<&Vector> for Vector {
    type Output = Vector;

    fn add(self, rhs: &Vector) -> Vector {
        &self + rhs
    }
}
impl ops::Add<Vector> for &Vector {
    type Output = Vector;

    fn add(self, rhs: Vector) -> Vector {
        self + &rhs
    }
}
impl ops::Add<Vector> for Vector {
    type Output = Vector;

    fn add(self, rhs: Vector) -> Vector {
        &self + &rhs
    }
}
impl ops::Sub<&Vector> for &Vector {
    type Output = Vector;

    fn sub(self, rhs: &Vector) -> Vector {
        Vector {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}
impl ops::Sub<&Vector> for Vector {
    type Output = Vector;

    fn sub(self, rhs: &Vector) -> Vector {
        &self - rhs
    }
}
impl ops::Sub<Vector> for &Vector {
    type Output = Vector;

    fn sub(self, rhs: Vector) -> Vector {
        self - &rhs
    }
}
impl ops::Sub<Vector> for Vector {
    type Output = Vector;

    fn sub(self, rhs: Vector) -> Vector {
        &self - &rhs
    }
}
impl ops::Mul<f64> for &Vector {
    type Output = Vector;

    fn mul(self, k: f64) -> Vector {
        Vector { 
            x: k * self.x,
            y: k * self.y,
        }
    }
}
impl ops::Mul<f64> for Vector {
    type Output = Vector;

    fn mul(self, k: f64) -> Vector {
        k * &self
    }
}
impl ops::Mul<&Vector> for f64 {
    type Output = Vector;

    fn mul(self, v: &Vector) -> Vector {
        Vector {
            x: self * v.x,
            y: self * v.y,
        }
    }
}
impl ops::Mul<Vector> for f64 {
    type Output = Vector;

    fn mul(self, v: Vector) -> Vector {
        self * &v
    }
}

impl ops::Neg for &Vector {
    type Output = Vector;

    fn neg(self) -> Vector {
        -1.0 * self
    }
}
impl ops::Neg for Vector {
    type Output = Vector;

    fn neg(self) -> Vector {
        -&self
    }
}

impl ops::Div<f64> for Vector {
    type Output = Vector;

    fn div(self, c: f64) -> Vector {
        (1.0 / c) * self
    }
}
impl ops::Div<f64> for &Vector {
    type Output = Vector;

    fn div(self, c: f64) -> Vector {
        (1.0 / c) * self
    }
}

impl Vector {
    pub fn new(x: f64, y: f64) -> Vector {
        Vector {x, y}
    }
    pub fn get_x(&self) -> f64 {
        self.x
    }
    pub fn get_y(&self) -> f64 {
        self.y
    }
    pub fn add(&mut self, other: &Vector) {
        self.x = self.x + other.x;
        self.y = self.y + other.y;
    }
    pub fn sub(&mut self, other: &Vector) {
        self.x = self.x - other.x;
        self.y = self.y - other.y;
    }
    pub fn dot(&self, other: &Vector) -> f64 {
        self.x * other.x + self.y * other.y
    }
    pub fn cross(&self, other: &Vector) -> f64 {
        return self.x * other.y - self.y * other.x;
    }
    pub fn dist2(&self) -> f64 {
        self.dot(self)
    }
    pub fn dist(&self) -> f64 {
        self.dist2().sqrt()
    }
    pub fn norm(&self) -> Vector {
        let d = self.dist();
        if d == 0.0 {
            return self.clone();
        }
        let scala = 1.0 / d;
        scala * self
    }
    pub fn rotate(&self, theta: f64) -> Vector {
        Vector {
            x: theta.cos() * self.x - theta.sin() * self.y,
            y: theta.sin() * self.x + theta.cos() * self.y,
        }
    }
    pub fn is_in_rect(&self, left_top: &Vector, right_bot: &Vector) -> bool {
        let x = left_top.x <= self.x && self.x < right_bot.x;
        let y = left_top.y <= self.y && self.y < right_bot.y;
        return x && y;
    }
    // selfが基準
    // selfはnorm
    // 0-2pi
    pub fn calc_angle(&self, other: &Vector) -> f64 {
        let other = &other.norm();
        let mut theta = self.dot(other).acos();
        if self.cross(other) < 0.0 {
            theta = 2.0 * PI - theta;
        }
        return theta;
    }
    pub fn most_similar(&self, others: &Vec<Vector>) -> (f64, Vector) {
        let mut max_simi = -1000.0;
        let mut max_simi_vec = Vector::new(0.0, 0.0);
        for v in  others.iter() {
            let simi = self.dot(v);
            if simi > max_simi {
                max_simi = simi;
                max_simi_vec = v.clone();
            }
        }
        return (max_simi, max_simi_vec);
    }
    pub fn basic_tetra() -> Vec<Vector> {
        let x_axis = Vector::new(1.0, 0.0);
        vec![
            x_axis.rotate(PI / 6.0),
            x_axis.rotate(5.0 * PI / 6.0),
            x_axis.rotate(3.0 * PI / 2.0 - PI / 12.0),
            x_axis.rotate(3.0 * PI / 2.0 + PI / 12.0),
        ]
    }
}

impl Into<iced::Point> for &Vector {
    fn into(self) -> iced::Point {
        iced::Point {x: self.x as f32, y: self.y as f32}
    }
}
impl Into<iced::Point> for Vector {
    fn into(self) -> iced::Point {
        (&self).into()
    }
}
impl From<iced::Point> for Vector {
    fn from(v: iced::Point) -> Self {
        Vector::new(v.x as f64, v.y as f64)
    }
}
