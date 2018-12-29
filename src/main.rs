use std::{self,io,io::Write};use rand::{Rng,FromEntropy};use rand::rngs::SmallRng
;type F=f32;
#[derive(Debug, Copy, Clone)]
struct V(F,F,F);impl V{fn d(&self,o:V)->F{self.0*o.0+self.1*o.1+self.2*o.2}fn n(
&self)->V{self*self.d(*self).sqrt().recip()}}fn v1(f:F)->V{V(f,f,f)}

macro_rules! bin_op {
    ( $trait:ident, $name:ident, $op:tt, $lhs:ty, $rhs:ty ) => {
        impl std::ops::$trait<$rhs> for $lhs {
            type Output = V;
            fn $name(self, o:$rhs)->V{V(
             self.0 $op o.0,
             self.1 $op o.1,
             self.2 $op o.2)
            }
        }
    }
}


macro_rules! scal_op {
    ( $trait:ident, $name:ident, $op:tt, $lhs:ty ) => {
        impl std::ops::$trait<F> for $lhs {
            type Output = V;
            fn $name(self, o:F)->V{V(
             self.0 $op o,
             self.1 $op o,
             self.2 $op o)
            }
        }
    }
}

macro_rules! op {
    ( $trait:ident, $name:ident, $op:tt ) => {
      bin_op!($trait,$name,$op,V,V);
      bin_op!($trait,$name,$op,V,&V);
      bin_op!($trait,$name,$op,&V,V);
      bin_op!($trait,$name,$op,&V,&V);
      scal_op!($trait,$name,$op,V);
      scal_op!($trait,$name,$op,&V);
    };
}

op!(Add,add,+);
op!(Sub,sub,-);
op!(Mul,mul,*);

impl std::ops::AddAssign<V> for V {
    fn add_assign(&mut self, o: V) {
        self.0 += o.0;
        self.1 += o.1;
        self.2 += o.2;
    }
}

impl std::ops::AddAssign<&V> for V {
    fn add_assign(&mut self, o: &V) {
        self.0 += o.0;
        self.1 += o.1;
        self.2 += o.2;
    }
}

fn bt(p:&V,l:&V,h:&V)->F{let l=p-l;let h=h-p;-l.0.min(h.0).min(l.1.min(h.1)).min(l.2.min(h.2))}

fn s(pos:V)->(F,i32){
    let mut hit = 1; // unless overridden by walls/sun below
    let mut dist : F = 1e9;

    // Letters
    {
        // Distance to letter segments in X-Y plane
        let origin = V(pos.0,pos.1,0.0);
        let data : &'static[u8; 15*4] = b"5O5_5W9W5_9_AOEOCOC_A_E_IOQ_I_QOUOY_Y_]OWW[WaOa_aWeWa_e_cWiO";
        for i in 0..15 {
            let begin = V(data[i*4] as F - 79.0,  data[i*4+1] as F - 79.0,  0.0) * 0.5;
            let end = V(data[i*4+2] as F - 79.0,  data[i*4+3] as F - 79.0,  0.0) * 0.5;
            let delta = &end - &begin;
            let d = (&begin - &origin).d(delta) / delta.d(delta);
            let v = &origin - &(&begin + &delta * (-d.min(0.0)).min(1.0));
            dist = dist.min(v.d(v));
        }
        dist = dist.sqrt();

        // Curves for P & R, hard-coded, also in X-Y plane
        let curves : [V; 2] = [
            V(-11.,6.,0.),
            V(11.,6.,0.),
        ];
        for c in &curves {
            let mut o = &origin - c;
            let curve_dist : F;
            if o.0 > 0.0 {
                curve_dist = o.d(o).sqrt() - 2.;
            }
            else {
                o.1+=if o.1>0.{-2.}else{0.};
                curve_dist = o.d(o).sqrt(); 
            }
            dist = dist.min(curve_dist);
        }
        
        // Letter 3D-ifying and "rounding"
        dist = (dist.powf(8.0) + pos.2.powf(8.0)).powf(0.125) - 0.5;
    }
    
    // Room
    {
        let carved_box_room_dist = bt(&pos, &V(-30.0, -0.5, -30.0), &V(30.0, 18.0, 30.0));
        let carved_ceiling_dist = bt(&pos, &V(-25.0, 17.0, -25.0), &V(25.0, 20.0, 25.0));
        let ceiling_beams_dist = bt(
                &V(
                     (pos.0.abs() / 8.0).fract() * 8.0, // |x| % 8
                    pos.1,pos.2
                ),
                &V(1.5, 18.5, -25.0),
                &V(6.5, 20.0, 25.0)
            );
        let room_dist = (-carved_box_room_dist.min(carved_ceiling_dist)).min(ceiling_beams_dist);
        if room_dist < dist {
            dist = room_dist;
            hit = 2;
        }
    }

    // Everything above 19.9 is sun
    {
        let sun_dist = 19.9 - pos.1;
        if sun_dist < dist {
            dist = sun_dist;
            hit = 3;
        }
    }

    return (dist, hit);
}

struct Hit {
    pos : V,
    norm : V
}

fn ray_march(p:&V,v:&V, hit: &mut Hit)->i32{
    let mut c=0;
    let mut t:F=0.;
    while t<100.{
        hit.pos=p+v*t;
        let (d,h) = s(hit.pos);
        c+=1;
        if (d<0.01) || (c > 99) {
            let nx = s(hit.pos+V(0.01,0.,0.)).0-d;
            let ny = s(hit.pos+V(0.,0.01,0.)).0-d;
            let nz = s(hit.pos+V(0.,0.,0.01)).0-d;
            hit.norm = V(nx,  ny,  nz).n();
            return h;
        }
        t += d;
    }
    return 0;
}

fn trace<R: Rng + ?Sized>(pos: &V, dir: &V, rng: &mut R)->V {
    let mut origin = *pos;
    let mut direction = *dir;
    let mut hit = Hit{ pos: origin, norm: origin }; // useless init
    let mut c = v1(0.0);
    let mut a = v1(1.0);
    let light_dir = V(0.6,  0.6,  1.0).n();
    for _ in 0..3{
        match ray_march(&origin, &direction, &mut hit) {
            1 => {
                direction += hit.norm * (-2.0 * hit.norm.d(direction));
                origin = hit.pos + direction * 0.1;
                a = &a * 0.2;
            },
            2 => {
                let incidence = hit.norm.d(light_dir);
                let p = 6.283185 * rng.gen::<F>();
                let q = rng.gen::<F>();
                let s = (1.0 - q).sqrt();
                let g = if hit.norm.2 < 0.0 { -1.0 } else { 1.0 };
                let u = -1.0 / (g + hit.norm.2);
                let v = hit.norm.0 * hit.norm.1 * u;
                direction = V(
                     v,
                     g + hit.norm.1 * hit.norm.1 * u,
                     -hit.norm.1
                 ) * (p.cos() * s)
                +
                V(
                     1.0 + g * hit.norm.0 * hit.norm.0 * u,
                     g * v,
                     -g * hit.norm.0
                 ) * (p.sin() * s)
                +
                hit.norm * q.sqrt();
                origin = &hit.pos + &direction * 0.1;
                a = &a * 0.2;
                if incidence > 0.0 {
                    if let 3 = ray_march(&(&origin + &(&hit.norm * 0.1)), &light_dir, &mut hit) {
                        c += &a * &V(500.0,  400.0,  100.0) * incidence;
                    }
                }
            },
            3 => {
                c += &a * &V(50.0,  80.0,  100.0);
                break;
            },
            _ => break
        }
    }
    return c;
}

fn main(){
    let width : i32 = 192; //960;
    let height : i32 = 108; //540;
    let samplecount = 16;

    let position = V(-22.0, 5.0, 25.0);
    let goal = (&V(-3.0, 4.0, 0.0) - &position).n();
    let left = V(goal.2, 0.0, -goal.0).n() * (1.0 / (width as F));
    let up = V(
         goal.1 * left.2 - goal.2 * left.1,
         goal.2 * left.0 - goal.0 * left.2,
         goal.0 * left.1 - goal.1 * left.0
    );

    let mut rng = SmallRng::from_entropy();

    print!("P6 {} {} 255 ", width, height);
    let sample_norm = 1.0 / (samplecount as F);
    let sample_bias = 14.0 / 241.0;
    let mut arr : Vec<u8> = Vec::with_capacity((width * height * 3) as usize);
    for y in (0..height).rev() {
        let fy0 : F = (y - height / 2) as F;
        for x in (0..width).rev() {
            let mut c = V(0.0, 0.0, 0.0);
            let fx0 : F = (x - width / 2) as F;
            for _ in 0..samplecount {
                let fx : F = rng.gen();
                let fy : F = rng.gen();
                let dir = (goal + left * (fx0 + fx) + up * (fy0 + fy)).n();
                c += trace(&position, &dir, &mut rng);
            }
            c = c * sample_norm + sample_bias;
            let den = &c + 1.0;
            c.0 *= 255.0 / den.0;
            c.1 *= 255.0 / den.1;
            c.2 *= 255.0 / den.2;
            arr.push(c.0 as u8);
            arr.push(c.1 as u8);
            arr.push(c.2 as u8);
        }
    }
    io::stdout().write_all(&arr[..]).expect("");
}
