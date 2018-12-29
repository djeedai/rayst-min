use std::{self,io,io::Write};use rand::{Rng,FromEntropy};use rand::rngs::SmallRng
;type F=f32;
#[derive(Debug, Copy, Clone)]
struct V(F,F,F);impl V{fn d(&self,o:V)->F{self.0*o.0+self.1*o.1+self.2*o.2}fn n(
&self)->V{self*self.d(*self).sqrt().recip()}}fn v1(f:F)->V{V(f,f,f)}fn v2(x:F,y:
F)->V{V(x,y,0.)}

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

op!(Add,add,+);op!(Sub,sub,-);op!(Mul,mul,*);op!(Div,div,/);

fn k(p:V,l:V,h:V)->F{let l=p-l;let h=h-p;-l.0.min(h.0).min(l.1.min(h.1)).min(l.2.min(h.2))}

fn s(p:V)->(F,i32){let mut h=1;let mut d:F=1e9;let o=v2(p.0,p.1);
let l=b"5O5_5W9W5_9_AOEOCOC_A_E_IOQ_I_QOUOY_Y_]OWW[WaOa_aWeWa_e_cWiO";
for i in 0..15{
let b=v2(l[i*4]as F-79.,l[i*4+1]as F-79.)*0.5;
let e=v2(l[i*4+2]as F-79.,l[i*4+3]as F-79.)*0.5-b;
let v=o-(b+e*(-((b-o).d(e)/e.d(e)).min(0.)).min(1.));
d=d.min(v.d(v));}d=d.sqrt();
for c in&[v2(-11.,6.),v2(11.,6.)]{let mut o=o-c;
let g:F;if o.0>0.{g=o.d(o).sqrt()-2.;}else{o.1+=-o.1.signum()-1.;g=o.d(o).sqrt();}d=d.min(g);}
d=(d.powf(8.)+p.2.powf(8.)).powf(0.125)-0.5;
let r=(-k(p,V(-30.,-0.5,-30.),V(30.,18.,30.)).min(
k(p,V(-25.,17.,-25.),V(25.,20.,25.)))).min(k(V((p.0.abs()/8.).fract()*8.,p.1,p.2),V(1.5,18.5,-25.),V(6.5,20.,25.)));
if r<d{d=r;h=2;}let s=19.9-p.1;if s<d{d=s;h=3;}return(d,h);}

fn ray_march(o:V,v:V)->(i32,V,V){let mut c=0;let mut t:F=0.;while t<1e2{let p=o+v*t;
let(d,h)=s(p);c+=1;if(d<0.01)||(c>99){return(h,p,V(s(p+v2(0.01,0.)).0-d,
s(p+v2(0.,0.01)).0-d,s(p+V(0.,0.,0.01)).0-d).n());}t+=d;}return(0,v1(0.),v1(0.));}

fn t<R:Rng+?Sized>(e:V,f:V,w:&mut R)->V{
let mut o=e;let mut d=f;let mut c=v1(0.);let mut a=v1(1.);let l=V(0.6,0.6,1.).n();
for _ in 0..3{let (t,p,n)=ray_march(o,d);match t{1=>{d=d+n*-2.*n.d(d);o=p+d*0.1;a=a*0.2;},
2=>{let i=n.d(l);let r=6.283185*w.gen::<F>();
let q:F=w.gen();let s=(1.-q).sqrt();let g=n.2.signum();let u=-1./(g+n.2);
let v=n.0*n.1*u;d=V(v,g+n.1*n.1*u,-n.1)*r.cos()*s+V(1.+g*n.0*n.0*u,g*v,-g*n.0)*r.sin()*s+n*q.sqrt();
o=p+d*0.1;a=a*0.2;if i>0.{if let 3=ray_march(o+n*0.1,l).0{c=c+a*V(5e2,4e2,1e2)*i;}
}},3=>{c=c+a*V(5e1,8e1,1e2);break;},_=>break}}return c;}

fn main(){let w:i32=192;let h:i32=108;let s=16;
    let p=V(-22.,5.,25.);let g=(v2(-3.,4.)-p).n();
    let l=V(g.2,0.,-g.0).n()*(1./(w as F));
    let u=V(g.1*l.2-g.2*l.1,g.2*l.0-g.0*l.2,g.0*l.1-g.1*l.0);
    let mut r = SmallRng::from_entropy();
    print!("P6 {} {} 255 ",w,h);
    let mut a : Vec<u8> = Vec::with_capacity((w * h * 3) as usize);
    for j in(0..h).rev(){for i in(0..w).rev(){
            let mut c=v1(0.);for _ in 0..s{
                c=c+t(p,(g+l*((i-w/2)as F+r.gen::<F>())+u*((j-h/2)as F+r.gen::<F>())).n(),&mut r);
            }c=c*(1./(s as F))+14./241.;c=c/(c+1.)*255.;
            a.write(&[c.0 as u8,c.1 as u8,c.2 as u8]).expect("");}}
    io::stdout().write_all(&a[..]).expect("");
}
