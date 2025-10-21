from helper_classes import *
import matplotlib.pyplot as plt

def render_scene(camera, ambient, lights, objects, screen_size, max_depth):
    width, height = screen_size
    ratio = float(width) / height
    screen = (-1, 1 / ratio, 1, -1 / ratio)  # left, top, right, bottom

    image = np.zeros((height, width, 3))

    for i, y in enumerate(np.linspace(screen[1], screen[3], height)):
        for j, x in enumerate(np.linspace(screen[0], screen[2], width)):
            # screen is on origin
            pixel = np.array([x, y, 0])
            origin = camera
            direction = normalize(pixel - origin)
            ray = Ray(origin, direction)

            color = np.zeros(3)

            # This is the main loop where each pixel color is computed.
            # TODO
            color = get_color_from_ray(ray, objects, ambient, lights, 1, max_depth)
            
            # We clip the values between 0 and 1 so all pixel values will make sense.
            image[i, j] = np.clip(color,0,1)

    return image

def get_color_from_ray(ray, objects, ambient, lights, level, max_level, ray_origin_object=None):
    color = np.zeros(3)
    
    obj, dis = ray.nearest_intersected_object([o for o in objects if o != ray_origin_object])
    if dis < 0 or dis >= math.inf:
        return np.zeros(3)
    
    intersection_point = ray.origin + (normalize(ray.direction) * dis)
    color += get_ambient_light(obj, ambient)
    visible_lights = [light for light in lights if is_light_visible(intersection_point, light, objects, obj)]
    color += get_diffuse_light(ray, obj, visible_lights, intersection_point)
    color += get_spectral_light(ray, obj, visible_lights, intersection_point)

    if level < max_level:
        reflected_ray = Ray(intersection_point, reflected(normalize(ray.direction), obj.get_forward_facing_normal(ray, intersection_point)))
        color += obj.reflection * get_color_from_ray(reflected_ray, objects, ambient, lights, level + 1, max_level, obj)

    return color


def is_light_visible(point, light, objects, intersected_object):
    ray_to_light = light.get_light_ray(point)
    obj, dis = ray_to_light.nearest_intersected_object([obj for obj in objects if obj != intersected_object])

    if dis == np.infty:
        return True

    return dis > light.get_distance_from_light(point)
    


def get_spectral_light(ray, obj, lights, intersection_point):
    color = np.zeros(3)

    for light in lights:
        normal = obj.get_forward_facing_normal(ray, intersection_point)
        v = -ray.direction
        r = reflected(-normalize(light.get_light_ray(intersection_point).direction), normal)
        v_dot_r = np.dot(v, r)
        v_dot_r = max(v_dot_r, 0)
        color += np.multiply(obj.specular, (v_dot_r ** obj.shininess) * light.get_intensity(intersection_point))

    return color


def get_diffuse_light(ray, obj, lights, intersection_point):
    color = np.zeros(3)

    for light in lights:
        normal = obj.get_forward_facing_normal(ray, intersection_point)
        l = normalize(light.get_light_ray(intersection_point).direction)
        normal_dot_l = np.dot(normal, l)
        normal_dot_l = max(normal_dot_l, 0)

        color += np.multiply(obj.diffuse, normal_dot_l * light.get_intensity(intersection_point))
    
    return color



def get_ambient_light(object, ambient):
    return object.ambient * ambient


def get_colors_for_rays(self, rays, lighting_func, max_depth):
    intersections = np.array([[obj.intersect(ray) for obj in self.objects] for ray in rays], dtype='object')
    found_objects_idx = np.argmin(intersections[:, :, 0], axis=1)

    intersections = np.array([intersections[i, found] for i, found in enumerate(found_objects_idx)], dtype='object')

    obj_coefficients = np.array(
        [np.array(obj.get_coefficients(), dtype='object') for
            obj in intersections[:, 1]], dtype='object')
    # do you consent
    img_coefficients = np.apply_along_axis(lambda o: lighting_func(o[0], o[1], self, max_depth), 1, list(zip(intersections, rays)))
    img = (img_coefficients * obj_coefficients)
    return [pix.sum() for pix in img]

# Write your own objects and lights
# TODO
def your_own_scene():
    sphere_a = Sphere([0, -0.5, -0.5],0.5)
    sphere_a.set_material([0.6, 1, 0.2], [0.6, 1, 0.2], [0.3, 0.3, 0.3], 100, 1)
    sphere_b = Sphere([0, 0.3, -0.5],0.3)
    sphere_b.set_material([0.1, 0, 1], [0.1, 0, 1], [0.3, 0.3, 0.3], 100, 0.2)
    plane = Plane([0,1,0], [0,-0.8,0])
    plane.set_material([0.5, 0.7, 1], [0.5, 0.7, 1], [1, 1, 1], 1000, 0.5)
    background = Plane([0,0,1], [0,0,-3])
    background.set_material([0.2, 0.2, 0.2], [0.2, 0.2, 0.2], [0.2, 0.2, 0.2], 1000, 0.5)

    v_list = np.array([[-0.5,0.6,-0.5],
                   [0.5,0.6,-0.5],
                   [0,1,-0.5]])

    triangle = Triangle(*v_list)
    triangle.set_material([1, 1, 0], [1, 1, 0], [0, 0, 0], 100, 0.5)


    objects = [sphere_a,sphere_b,plane,background, triangle]

    light_0 = PointLight(intensity= np.array([1, 1, 1]),position=np.array([1,1.5,1]),kc=0.1,kl=0.1,kq=0.1)
    light_1 = SpotLight(intensity= np.array([1, 1, 1]), position=np.array([0,5,0]),direction=[0,0,1] , kc=0.1,kl=0.1,kq=0.1)

    lights = [light_0, light_1]
    camera = np.array([0,0,1])   
    return camera, lights, objects
