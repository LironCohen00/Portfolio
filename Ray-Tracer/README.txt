Ray Tracer
==========

A ray tracing renderer implemented in Python.

Rendering pipeline:
- For each pixel, a ray is cast from the camera through the screen plane into
  the scene.
- The nearest intersected object is found by testing the ray against all scene
  geometry.
- Shading is computed using the Phong illumination model:
    • Ambient: constant base light applied to all surfaces
    • Diffuse: light intensity proportional to the angle between the surface
      normal and the light direction (Lambertian reflectance)
    • Specular: highlight computed from the angle between the reflected ray and
      the camera direction
- Shadow rays are cast from each intersection point toward every light source;
  if another object is in the way, that light's contribution is skipped.
- Reflections are handled recursively: a reflected ray is spawned from the
  intersection point and the returned colour is blended in, up to a configurable
  maximum recursion depth.

Light types supported: DirectionalLight, PointLight (with quadratic attenuation),
SpotLight.

Contents:
- Main.py                          — rendering loop and shading logic
- helper_classes.py                — scene objects, vectors, rays, and lighting classes
- Ray Tracing Expirements.ipynb    — Jupyter notebook with rendered output

Language: Python
Dependencies: NumPy, matplotlib
