use std::iter::Iterator;

pub struct LinearResampler {
    from_sample_rate: u32,
    to_sample_rate: u32,

    current_from_sample: f32,
    next_from_sample: f32,
    from_fract_pos: u32,
}

impl LinearResampler {
    pub fn new(from_sample_rate: u32, to_sample_rate: u32) -> LinearResampler {
        let sample_rate_gcd = {
            fn gcd(a: u32, b: u32) -> u32 {
                if b == 0 {
                    a
                } else {
                    gcd(b, a % b)
                }
            }

            gcd(from_sample_rate, to_sample_rate)
        };

        LinearResampler {
            from_sample_rate: from_sample_rate / sample_rate_gcd,
            to_sample_rate: to_sample_rate / sample_rate_gcd,

            current_from_sample: 0.0,
            next_from_sample: 0.0,
            from_fract_pos: 0,
        }
    }

    pub fn next(&mut self, input: &mut dyn Iterator<Item = f32>) -> f32 {
        fn interpolate(a: f32, b: f32, num: u32, denom: u32) -> f32 {
            (a * ((denom - num) as f32) + b * (num as f32)) / (denom as f32)
        }

        let ret = interpolate(
            self.current_from_sample,
            self.next_from_sample,
            self.from_fract_pos,
            self.to_sample_rate,
        );

        self.from_fract_pos += self.from_sample_rate;
        while self.from_fract_pos > self.to_sample_rate {
            self.from_fract_pos -= self.to_sample_rate;

            self.current_from_sample = self.next_from_sample;
            self.next_from_sample = input.next().unwrap_or(self.current_from_sample);
        }

        ret
    }
}
