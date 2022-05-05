let success: bool;
let done: bool = false;
let i: i32;
let i: i32; let j: i32;
let j: i32 = 1;
let k: u32 = 3;
let x: f32 = 1.0;
let a: f32 = 1.5; let b: f32;
let texcoord1: vec2<f32>; let texcoord2: vec2<f32>;
let position: vec3<f32>;
let myRGBA: vec4<f32>;
let textureLookup: vec2<i32>;
let less: vec3<bool>;
let mat2D: mat2x2<f32>;
let optMatrix: mat3x3<f32>;
let view: mat4x4<f32>;
let m: mat3x2<f32>;
struct Light
{
  intensity: f32;
  position: vec3<f32>;
};
let lightVar: light;
let frequencies: f32[3];
let lightPosition: var<uniform> vec4<f32>[4];
let lights: light[];
let numLights: const i32 = 2;
let lights: light[numLights];
let a: i32[5];
let coef: const f32 = 2.75;
fn f ()  ->  i32;
fn foo ()  ->  f32[5];
fn foo (:  f32[5]) ;
fn foo (a:  f32[5]) ;
let zAxis: const vec3<f32> = vec3<f32> (0.0, 0.0, 1.0);
let position: in vec4<f32>;
let normal: in vec3<f32>;
let texCoord: in vec2<f32>[4];
let foo: in f32[];
let TexCoord: centroid out vec2<f32>;
let Color: invariant centroid out vec4<f32>;
let Color: invariant flat centroid out vec4<f32>;
let temperature: noperspective out f32;
let myColor: flat out vec3<f32>;
let myTexCoord: noperspective centroid out vec2<f32>;
let FragmentColor: out vec4<f32>;
let Luminosity: out u32;
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
let gl_FragCoord: @origin_upper_left in vec4<f32>;
let gl_FragCoord: @pixel_center_integer in vec4<f32>;
let gl_FragCoord: @origin_upper_left pixel_center_integer in vec4<f32>;
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
let gl_FrontColor: flat out vec4<f32>;
let color: lowp f32;
let P: out mediump vec2<f32>;
fn foo (:  lowp mat3x3<f32>)  ->  lowp vec2<i32>;
let m: highp mat4x4<f32>;
precision highp f32;
precision highp i32;
precision mediump i32;
invariant gl_Position;
let Color: out vec3<f32>;
invariant Color;
let Color: invariant centroid out vec3<f32>;
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
  pattern: f32[100];
  arr: f32[];
}
@std140 var<uniform>: PatternBlock;
let x: f32 = a / b / c;
let x: f32 = a / b / c;
let x: f32 = a / (b / c);
