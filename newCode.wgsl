fn vmax (v: vec2<f32>)  ->  f32
{
  return max(v.x, v.y);
}
fn fBox2 (p: vec2<f32>, b: vec2<f32>)  ->  f32
{
  return vmax(abs(p) - b);
}
fn rot (a: f32)  ->  mat2x2<f32>
{
  let s: f32 = sin(a);
  let c: f32 = cos(a);
  return mat2x2<f32>(c, s, -s, c);
}
fn wf1 (p: vec2<f32>)  ->  f32
{
  return sin(p.x) + cos(p.y);
}
fn cappedCylinder (p: vec3<f32>, h: f32, r: f32)  ->  f32
{
  let d: vec2<f32> = abs(vec2<f32>(length(p.xz), p.y)) - vec2<f32>(r, h);
  return min(max(d.x, d.y), 0.0) + length(max(d, 0.0));
}
var gl: vec3<f32> = vec3<f32>(0.0);
var gl1: f32 = 0.0;
var gl2: vec3<f32> = vec3<f32>(0.0);
let gl22: vec3<f32> = vec3<f32>(0.0);
fn map (p: vec3<f32>)  ->  vec3<f32>
{
  gl1 = 2;
  var r: vec3<f32> = vec3<f32>(0.0);
  let d: vec3<f32> = vec3<f32>(0.0);
  p.xz =  p.xz * rot(iTime * 0.5);
  let m: vec3<f32> = p;
  p.xz =  p.xz * rot(sin(-p.y * 0.5) * 1.1);
  p.xz = abs(p.zx) - vec2<f32>(0.8);
  let i: f32 = sin(p.y * 3.0 + iTime * 10.0) * 0.5 + 0.5;
  let b: f32 = cappedCylinder(p, 5.5, (i - 0.5) * 2.0 * 0.3 * cos(p.y * 0.2));
  gl =  gl + 4.0e-4 / (1.0e-2 + b * b) * mix(vec3<f32>(1.0, 0.0, 1.0), vec3<f32>(1.0, 1.0, 0.0), p.y);
  r.x = max(cappedCylinder(p, 2.0, 0.3 + 0.2 * i), -cappedCylinder(p, 3.0, 0.2 + 0.25 * i));
  p.xz =  p.xz * rot(p.y * 3.0 + iTime * 2.0);
  var q: vec3<f32> = p;
  q.xz =  q.xz * rot(3.14 / 2.0);
  if (fBox2(p.xy, vec2<f32>(0.2, 10.0)) < 0.0) {
    r.yz = vec2<f32>(3.0, 0.0);
  }
  else {
    if (fBox2(q.xy, vec2<f32>(0.2, 10.0)) < 0.0) {
      r.yz = vec2<f32>(4.0, 0.0);
    }
    else {
      r.yz = vec2<f32>(1.0);
    }
  }
  gl1 =  gl1 + 1.0e-6 / (1.0e-6 + pow(r.x + 3.0e-3, 2.0));
  d.x = min(r.x, cappedCylinder(p, 8.5, (0.25 + (i - 0.5) * 2.0 * 0.15) * cos(p.y * 0.2)));
  d.y = 2.0;
  if (r.x > d.x) {
    r = d;
  }
  p = m;
  d.x = length(p) - 0.45 - 0.1 * (sin(iTime * 10.0) * 0.5 + 0.5);
  gl2 =  gl2 + 6.0e-4 / (1.0e-2 + d.x * d.x) * mix(vec3<f32>(1.0, 0.0, 1.0), vec3<f32>(1.0, 1.0, 0.0), m.y);
  if (r.x > d.x) {
    r = d;
  }
  p = m;
  if (p.y > 0.0) {
    p.xz =  p.xz * rot(0.3);
  }
  p = abs(p);
  p.zx =  p.zx * rot(-3.14 / 4.0);
  p.xy =  p.xy * rot(-3.14 / 4.0);
  p.y =  p.y - 1.0;
  q = p;
  p.yx =  p.yx * rot(sin(p.y * 3.14) * 0.3);
  d.x = cappedCylinder(p, 1.0, 6.0e-2 + (i - 0.5) * 2.0 * 4.0e-2);
  p = q;
  p.y =  p.y - 1.0;
  d.x = min(d.x, length(p) - 0.15 - 5.0e-2 * (sin(iTime * 10.0 + 1.5) * 0.5 + 0.5));
  gl2 =  gl2 + 3.0e-4 / (1.0e-2 + d.x * d.x) * mix(vec3<f32>(1.0, 0.0, 1.0), vec3<f32>(1.0, 1.0, 0.0), -m.y);
  d.y = 2.0;
  if (r.x > d.x) {
    r = d;
  }
  return r;
}
let e: const vec2<f32> = vec2<f32>(3.5e-4, -3.5e-4);
fn norm (po: vec3<f32>)  ->  vec3<f32>
{
  var what: i32 = 3;
  var what3: i32 = 3;
  what = 2;
  return normalize(e.yyx * map(po + e.yyx).x + e.yxy * map(po + e.yxy).x + e.xyy * map(po + e.xyy).x + e.xxx * map(po + e.xxx).x);
}
fn mainImage (outfragColor: vec4<f32>, infragCoord: vec2<f32>) 
{
  let uv: vec2<f32> = (fragCoord - iResolution.xy * 0.5) / iResolution.y;
  let ro: vec3<f32> = vec3<f32>(0.0, 3.0, -6.0);
  let rd: vec3<f32> = normalize(vec3<f32>(uv, 1.0));
  var p: vec3<f32>;
  var h: vec3<f32>;
  rd.yz =  rd.yz * rot(-0.4);
  var t: f32 = 0.0;
  for (let i: i32 = 0; ; i < 120 ; i ++)
  {
    p = ro + rd * t;
    h = map(p);
    if (h.x < 1.0e-4 || t > 40.0) {
      {
        if (h.z == 1.0) {
          h.x = abs(h.x) + 1.0e-4;
        }
        else {
          break;
        }
      }
    }
    ;
    t =  t + h.x * 0.7;
  }
  let ld: vec3<f32> = vec3<f32>(0.0, 1.0, 0.0);
  let ld1: vec3<f32> = vec3<f32>(3.0, 3.0, 0.0);
  ld1.xz =  ld1.xz * rot(iTime * 0.3);
  var col: vec3<f32> 