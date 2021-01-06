#version 330 core

out vec4 FragColor;

in vec3 vertex_world;
in vec3 tnorm;
in vec2 texture_coordinate;

in vec3 displacement;

uniform sampler2D texture_diffuse1;
uniform vec3 light_position;
uniform vec3 view_position;

vec3 La = vec3(0.08, 0.02, 0.9);    // ambient colour
vec3 Ka = vec3(1.0, 1.0, 1.0);    // ambient light reflectance
float ambient_strength = 0.2;

vec3 Ld = vec3(0.95, 0.4, 0.05);    // diffuse light colour
vec3 Kd = vec3(1.0, 1.0, 1.0);    // diffuse surface reflectance

vec3 Ls = vec3(0.95, 0.4, 0.05);    // specular colour
vec3 Ks = vec3(1.0, 1.0, 1.0);    // specular reflectance

float specular_exponent = 100.0;  // specular 'power'

void main()
{
  // problem is that when the object moves, the lights angle doesnt change
  
  vec3 light_direction = normalize(light_position - vertex_world);
  
//  vec3 light_direction = normalize(vertex_world - light_position );
//  vec3 view_direction = normalize(view_position - vertex_world);
  vec3 view_direction = normalize(vertex_world - view_position);                      
  vec3 reflection_direction = normalize(reflect(light_direction, tnorm));            
  
  // specular
  float specular_dot_product = max(dot(view_direction, reflection_direction), 0.0);  
  float specular_factor = pow(specular_dot_product, specular_exponent);
  vec3 Is = Ls * Ks * specular_factor;
  // diffuse
  // dot product on unit vectors  : v . k = cos(theta)
  // cos(0)  = 1
  // cos(90)= 0
  // we are getting the angle between the light direction and the normal of the surface
  float diffuse_dot_product = max(dot(tnorm, light_direction), 0.0);                  
  vec3 Id = Ld * Kd * diffuse_dot_product;
  // ambient
  vec3 Ia = (La * Ka) * ambient_strength;
    
  vec4 light_colour  = vec4(Is + Id + Ia, 1.0);
  vec4 model_colour  = texture(texture_diffuse1, texture_coordinate);
  FragColor = light_colour * model_colour;
}

