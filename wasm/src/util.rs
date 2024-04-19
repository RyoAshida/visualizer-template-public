#![allow(non_snake_case, unused_macros)]
use proconio::input;
use rand::prelude::*;
use svg::node::element::{Circle, Group, Definitions, Image, Line, Path, Rectangle, Text};
use web_sys::console::log_1;

#[derive(Debug, Clone)]
pub struct Input {
    pub n: usize,
    pub m: usize,
    pub a: Vec<usize>,
    pub b: Vec<usize>,
}

impl std::fmt::Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {}", self.n, self.m)?;
        for i in 0..self.n {
            writeln!(f, "{} {}", self.a[i], self.b[i])?;
        }
        Ok(())
    }
}

pub fn parse_input(f: &str) -> Input {
    let f = proconio::source::once::OnceSource::from(f);
    input! {
        from f,
        n: usize,
        m: usize,
        abs: [(usize, usize); n]
    }
    let a = abs.iter().map(|(a, _)| *a).collect::<Vec<_>>();
    let b = abs.iter().map(|(_, b)| *b).collect::<Vec<_>>();
    Input { n, m, a, b }
}

pub struct Output {
    pub c: Vec<usize>,
    pub d: Vec<usize>,
    pub v: usize,
    pub t: Vec<usize>,
    pub r: Vec<usize>,
}

pub fn parse_output(f: &str, m: usize) -> Output {
    let f = proconio::source::once::OnceSource::from(f);
    input! {
        from f,
        cd: [(usize, usize); m],
        v: usize,
        tr: [(usize, usize); v],
    }
    let c = cd.iter().map(|(c, _)| *c).collect::<Vec<_>>();
    let d = cd.iter().map(|(_, d)| *d).collect::<Vec<_>>();
    let t = tr.iter().map(|(t, _)| *t).collect::<Vec<_>>();
    let r = tr.iter().map(|(_, r)| *r).collect::<Vec<_>>();
    Output { c, d, v, t, r }
}

pub fn gen(seed: u64) -> Input {
    let mut rng = rand_chacha::ChaCha20Rng::seed_from_u64(seed);

    let mut uv: Vec<(usize, usize)> = vec![];
    while uv.len() < 15 {
        let u = rng.gen_range(100..=900);
        let v = rng.gen_range(100..=900);

        let mut flag = 0;
        for (uval, vval) in uv.iter() {
            // ユークリッド距離を計算する
            let dx = (*uval as i32 - u as i32).abs();
            let dy = (*vval as i32 - v as i32).abs();
            let dist = (dx * dx + dy * dy) as f64;
            if dist < 100. * 100. {
                flag = 1;
                break;
            }
        }
        if flag == 1 {
            continue;
        }
        uv.push((u, v));
    }

    let mut xy: Vec<(usize, usize)> = vec![];
    for i in 0..100 {
        let base = rng.gen_range(0..15);
        let x = uv[base].0 + rng.gen_range(0..=200) - 100;
        let y = uv[base].1 + rng.gen_range(0..=200) - 100;
        xy.push((x, y));
    }

    let a = xy.iter().map(|(a, _)| *a).collect::<Vec<_>>();
    let b = xy.iter().map(|(_, b)| *b).collect::<Vec<_>>();
    Input { n: 100, m: 8, a, b}
}

fn get_pos(input: &Input, output: &Output, turn: usize) -> (usize, usize) {
    if output.t[turn] == 1 {
        return (input.a[output.r[turn] - 1], input.b[output.r[turn] -1]);
    } else {
        return (output.c[output.r[turn] - 1], output.d[output.r[turn] - 1]);
    }
}

fn calculate_score(input: &Input, output: &Output) -> i64 {
    let mut result: f64 = 0.;

    let vv = output.v;
    for i in 0..(output.v - 1) {
        let base1 = output.t[i];
        let base2 = output.t[i+1];
        let mut coeff = 1;
        if base1 == 1 && base2 == 1 {
            coeff = 25;
        } else if (base1 == 1 && base2 == 2) || (base1 == 2 && base2 == 1) {
            coeff = 5;
        } else {
            coeff = 1;
        }
        let (x1, y1) = get_pos(input, output, i);
        let (x2, y2) = get_pos(input, output, i+1);
        let euclid = ((x1 as i32 - x2 as i32).abs().pow(2) + (y1 as i32 - y2 as i32).abs().pow(2)) as i32;
        result += (euclid * coeff) as f64;
    }

    let mut ret = 1000 * 1000 * 1000 / (1000 as f64 + (result as f64).powf(0.5)) as i64;
    return ret;
}

pub fn vis(input: &Input, output: &Output, turn: usize) -> (i64, String, String) {
    let score = calculate_score(input, output);

    let W = 1000;
    let H = 1000;
    let mut doc = svg::Document::new()
        .set("id", "vis")
        .set("viewBox", (-5, -5, W+5, H+5))
        .set("width", W+10)
        .set("height", H+10)
        .set("style", "background-color: white");

    for i in 0..input.n {
        doc = doc.add(
            Circle::new()
                .set("cx", input.a[i] as i32)
                .set("cy", input.b[i] as i32)
                .set("r", 8)
                .set("fill", "#ffff00")
                .set("stroke", "black")
                .set("stroke-width", 1)
                .set("class", "box")
        );
    }

    for i in 0..input.m {
        doc = doc.add(
            Circle::new()
                .set("cx", output.c[i] as i32)
                .set("cy", output.d[i] as i32)
                .set("r", 10)
                .set("fill", "#ff0000")
                .set("stroke", "black")
                .set("stroke-width", 1)
                .set("class", "box")
        );
    }

    for i in 0..(output.v - 1) {
        let (x1, y1) = get_pos(input, output, i);
        let (x2, y2) = get_pos(input, output, i+1);
        doc = doc.add(
            Line::new()
                .set("x1", x1 as i32)
                .set("y1", y1 as i32)
                .set("x2", x2 as i32)
                .set("y2", y2 as i32)
                .set("stroke", "black")
                .set("stroke-width", 1)
        );
    }

    let (x, y) = get_pos(input, output, turn);
    doc = doc.add(
        Circle::new()
            .set("cx", x as i32)
            .set("cy", y as i32)
            .set("r", 12)
            .set("fill", "#0000ff")
            .set("stroke", "black")
            .set("stroke-width", 1)
            .set("class", "box")
    );

    (score as i64, "".to_string(), doc.to_string())
}