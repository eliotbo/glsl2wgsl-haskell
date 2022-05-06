[Leto "fn vmax (v: vec2<f32>)  ->  f32\n{\n  return max(v.x, v.y);\n}\nfn fBox2 (p: vec2<f32>, b: vec2<f32>)  ->  f32\n{\n  return vmax(abs(p) - b);\n}\nfn rot (a: f32)  ->  mat2x2<f32>\n{\n  let" (-1),Leto " s: f32 = sin(a);\n  let" (-1),Leto " c: f32 = cos(a);\n  return mat2x2<f32>(c, s, -s, c);\n}\nfn wf1 (p: vec2<f32>)  ->  f32\n{\n  return sin(p.x) + cos(p.y);\n}\nfn cappedCylinder (p: vec3<f32>, h: f32, r: f32)  ->  f32\n{\n  let" (-1),Leto " d: vec2<f32> = abs(vec2<f32>(length(p.xz), p.y)) - vec2<f32>(r, h);\n  return min(max(d.x, d.y), 0.0) + length(max(d, 0.0));\n}\nlet" (-1),Leto " gl: vec3<f32> = vec3<f32>(0.0);\nlet" (-1),Leto " gl1: f32 = 0.0;\nlet" (-1),Leto " gl2: vec3<f32> = vec3<f32>(0.0);\nlet" (-1),Leto " gl22: vec3<f32> = vec3<f32>(0.0);\nfn map (p: vec3<f32>)  ->  vec3<f32>\n{\n  gl1 = 2;\n  let" (-1),Leto " r: vec3<f32> = vec3<f32>(0.0);\n  let" (-1),Leto " d: vec3<f32> = vec3<f32>(0.0);\n  p.xz =  p.xz * rot(iTime * 0.5);\n  let" (-1),Leto " m: vec3<f32> = p;\n  p.xz =  p.xz * rot(sin(-p.y * 0.5) * 1.1);\n  p.xz = abs(p.zx) - vec2<f32>(0.8);\n  let" (-1),Leto " i: f32 = sin(p.y * 3.0 + iTime * 10.0) * 0.5 + 0.5;\n  let" (-1),Leto " b: f32 = cappedCylinder(p, 5.5, (i - 0.5) * 2.0 * 0.3 * cos(p.y * 0.2));\n  gl =  gl + 4.0e-4 / (1.0e-2 + b * b) * mix(vec3<f32>(1.0, 0.0, 1.0), vec3<f32>(1.0, 1.0, 0.0), p.y);\n  r.x = max(cappedCylinder(p, 2.0, 0.3 + 0.2 * i), -cappedCylinder(p, 3.0, 0.2 + 0.25 * i));\n  p.xz =  p.xz * rot(p.y * 3.0 + iTime * 2.0);\n  let" (-1),Leto " q: vec3<f32> = p;\n  q.xz =  q.xz * rot(3.14 / 2.0);\n  if (fBox2(p.xy, vec2<f32>(0.2, 10.0)) < 0.0) {\n    r.yz = vec2<f32>(3.0, 0.0);\n  }\n  else {\n    if (fBox2(q.xy, vec2<f32>(0.2, 10.0)) < 0.0) {\n      r.yz = vec2<f32>(4.0, 0.0);\n    }\n    else {\n      r.yz = vec2<f32>(1.0);\n    }\n  }\n  gl1 =  gl1 + 1.0e-6 / (1.0e-6 + pow(r.x + 3.0e-3, 2.0));\n  d.x = min(r.x, cappedCylinder(p, 8.5, (0.25 + (i - 0.5) * 2.0 * 0.15) * cos(p.y * 0.2)));\n  d.y = 2.0;\n  if (r.x > d.x) {\n    r = d;\n  }\n  p = m;\n  d.x = length(p) - 0.45 - 0.1 * (sin(iTime * 10.0) * 0.5 + 0.5);\n  gl2 =  gl2 + 6.0e-4 / (1.0e-2 + d.x * d.x) * mix(vec3<f32>(1.0, 0.0, 1.0), vec3<f32>(1.0, 1.0, 0.0), m.y);\n  if (r.x > d.x) {\n    r = d;\n  }\n  p = m;\n  if (p.y > 0.0) {\n    p.xz =  p.xz * rot(0.3);\n  }\n  p = abs(p);\n  p.zx =  p.zx * rot(-3.14 / 4.0);\n  p.xy =  p.xy * rot(-3.14 / 4.0);\n  p.y =  p.y - 1.0;\n  q = p;\n  p.yx =  p.yx * rot(sin(p.y * 3.14) * 0.3);\n  d.x = cappedCylinder(p, 1.0, 6.0e-2 + (i - 0.5) * 2.0 * 4.0e-2);\n  p = q;\n  p.y =  p.y - 1.0;\n  d.x = min(d.x, length(p) - 0.15 - 5.0e-2 * (sin(iTime * 10.0 + 1.5) * 0.5 + 0.5));\n  gl2 =  gl2 + 3.0e-4 / (1.0e-2 + d.x * d.x) * mix(vec3<f32>(1.0, 0.0, 1.0), vec3<f32>(1.0, 1.0, 0.0), -m.y);\n  d.y = 2.0;\n  if (r.x > d.x) {\n    r = d;\n  }\n  return r;\n}\nlet" (-1),Leto " e: const vec2<f32> = vec2<f32>(3.5e-4, -3.5e-4);\nfn norm (po: vec3<f32>)  ->  vec3<f32>\n{\n  let" (-1),Leto " what: i32 = 3;\n  let" (-1),Leto " what3: i32 = 3;\n  what = 2;\n  return normalize(e.yyx * map(po + e.yyx).x + e.yxy * map(po + e.yxy).x + e.xyy * map(po + e.xyy).x + e.xxx * map(po + e.xxx).x);\n}\nfn mainImage (outfragColor: vec4<f32>, infragCoord: vec2<f32>) \n{\n  let" (-1),Leto " uv: vec2<f32> = (fragCoord - iResolution.xy * 0.5) / iResolution.y;\n  let" (-1),Leto " ro: vec3<f32> = vec3<f32>(0.0, 3.0, -6.0);\n  let" (-1),Leto " rd: vec3<f32> = normalize(vec3<f32>(uv, 1.0));\n  let" (-1),Leto " p: vec3<f32>;\n  let" (-1),Leto " h: vec3<f32>;\n  rd.yz =  rd.yz * rot(-0.4);\n  let" (-1),Leto " t: f32 = 0.0;\n  for (let" (-1),Leto " i: i32 = 0; ; i < 120 ; i ++)\n  {\n    p = ro + rd * t;\n    h = map(p);\n    if (h.x < 1.0e-4 || t > 40.0) {\n      {\n        if (h.z == 1.0) {\n          h.x = abs(h.x) + 1.0e-4;\n        }\n        else {\n          break;\n        }\n      }\n    }\n    ;\n    t =  t + h.x * 0.7;\n  }\n  let" (-1),Leto " ld: vec3<f32> = vec3<f32>(0.0, 1.0, 0.0);\n  let" (-1),Leto " ld1: vec3<f32> = vec3<f32>(3.0, 3.0, 0.0);\n  ld1.xz =  ld1.xz * rot(iTime * 0.3);\n  let" (-1),Leto " col: vec3<f32> = vec3<f32>(0.1);\n  if (h.x < 1.0e-4) {\n    {\n      if (h.y == 1.0) {\n        col = vec3<f32>(0.1, 0.3, 0.2);\n      }\n      if (h.y == 2.0) {\n        col = vec3<f32>(0.7, 0.7, 0.3);\n      }\n      if (h.y == 3.0) {\n        col = vec3<f32>(0.5, 0.9, 0.5);\n      }\n      if (h.y == 4.0) {\n        col = vec3<f32>(0.5, 0.5, 0.9);\n      }\n    }\n  }\n  col = mix(col, vec3<f32>(0.1, 0.3, 0.2), clamp(gl1, 0.0, 1.0));\n  col =  col + gl;\n  col =  col + gl2;\n  what3 = 5;\n  fragColor = vec4<f32>(col, 1.0);\n}\nlet" (-1),Leto " exit: let" (-1),Errco]