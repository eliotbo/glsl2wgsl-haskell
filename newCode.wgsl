let success:Just bool;
let done: bool = false;
let i:Just i32;
let i:Nothing; let j:Nothing;
let j: i32 = 1;
let k: u32 = 3;
let x: f32 = 1.0;
let a: f32 = 1.5; let b: f32 = 1.5;
let texcoord1:Nothing; let texcoord2:Nothing;
let position:Just vec3<f32>;
let myRGBA:Just vec4<f32>;
let textureLookup:Just vec2<i32>;
let less:Just vec3<bool>;
let mat2D:Just mat2x2<f32>;
let optMatrix:Just mat3x3<f32>;
let view:Just mat4x4<f32>;
let m:Just mat3x2<f32>;
let lightVar:Just struct light
                  { intensity: f32;
                    position: vec3<f32>;
                  };
let lightVar3:Just light;
let frequencies[3]TAAA: f32;
let lightPosition[4]TAAA: var<uniform> vec4<f32>;
let lights[]TAAA: light;
let numLights: const i32 = 2;
let lights[numLights]TAAA: light;
let a[5]TAAA: i32;
let coef: const f32 = 2.75;
fn f ()  ->  i32;
fn foo ()  ->  f32 [5];
fn foo (:  f32 [5]) ;
fn foo (a[5]:  f32) ;
let zAxis: const vec3<f32> = vec3<f32> (0.0, 0.0, 1.0);
let position:Just in vec4<f32>;
let normal:Just in vec3<f32>;
let texCoord[4]TAAA: in vec2<f32>;
let foo[]TAAA: in f32;
let TexCoord:Just centroid out vec2<f32>;
let Color:Just invariant centroid out vec4<f32>;
let Color:Just invariant flat centroid out vec4<f32>;
let temperature:Just noperspective out f32;
let myColor:Just flat out vec3<f32>;
let myTexCoord:Just noperspective centroid out vec2<f32>;
let FragmentColor:Just out vec4<f32>;
let Luminosity:Just out u32;
struct Transform
{
  ModelViewMatrix: mat4x4<f32>;
  ModelViewProjectionMatrix: mat4x4<f32>;
  var<uniform> NormalMatrix: mat3x3<f32>;
  Deformation: f32;
}
var<uniform>: Transform;
struct Material
{
  smooth in Color1: vec4<f32>;
  smooth Color2: vec4<f32>;
  TexCoord: vec2<f32>;
}
in: Material;
struct Vertex
{
  Position: vec4<f32>;
  Texture: vec2<f32>;
}
out Coords: Vertex;
struct Transform
{
  ModelViewMatrix: mat4x4<f32>;
  ModelViewProjectionMatrix: mat4x4<f32>;
  Deformation: f32;
}
var<uniform> transforms[4]: Transform;
@triangles in;
let gl_FragCoord:Just @origin_upper_left in vec4<f32>;
let gl_FragCoord:Just @pixel_center_integer in vec4<f32>;
let gl_FragCoord:Just @origin_upper_left pixel_center_integer in vec4<f32>;
@triangle_strip max_vertices(60) out;
@triangle_strip out;
@max_vertices(60) out;
@shared column_major var<uniform>;
struct Transform
{
  M1: mat4x4<f32>;
  @column_major M2: mat4x4<f32>;
  N1: mat3x3<f32>;
}
@std140 var<uniform>: Transform;
let gl_FrontColor:Just flat out vec4<f32>;
let color:Just lowp f32;
let P:Just out mediump vec2<f32>;
fn foo (:  lowp mat3x3<f32>)  ->  lowp vec2<i32>;
let m:Just highp mat4x4<f32>;
precision highp f32;
precision highp i32;
precision mediump i32;
invariant gl_Position;
let Color:Just out vec3<f32>;
invariant Color;
let Color:Just invariant centroid out vec3<f32>;
let color: vec4<f32> = vec4<f32> (0.0, 1.0, 0.0, 1.0);
fn main () 
{
 
}
let i: i32 = 1 - 5 * 4 + 3;
let i: i32 = 1 - 5 * 4 + 3;
let i: i32 = (1 - 5) * 4 + 3;
let i: i32 = (1 - 5) * (4 + 3);
let b: bool = 1 < 2;
fn main () 
{
  if (intensity < 0.0)
  {
  	 return;
  }
  if (a & b)
  {
  	 return;
  }
  if (a | b)
  {
  	 return;
  }
  if (a && b)
  {
  	 return;
  }
  if (a || b)
  {
  	 return;
  }
}
struct PatternBlock
{
  pattern[100]: f32;
  arr[]: f32;
}
@std140 var<uniform>: PatternBlock;
let x: f32 = a / b / c;
let x: f32 = a / b / c;
let x: f32 = a / (b / c);
