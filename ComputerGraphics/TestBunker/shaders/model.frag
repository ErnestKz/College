#version 330 core
out vec4 FragColor;

in vec2 TexCoords;

in vec3 position_eye, normal_eye, N;
in mat4 view_matrix;
in vec3 light_position_world;
in vec3 vertex_world;


uniform sampler2D texture_diffuse1;

void main()
{

  vec3 light_position_eye = vec3(view_matrix * vec4(light_position_world, 1.0));
  vec3 L = normalize(position_eye - vertex_world);
  float dot_prod = max(dot(L,N), 0.0);
  vec3 R = reflect(-L, N);
  vec3 V = normalize(-position_eye); // assumes camera is at origin
  float dot_prod_specular = max(dot(R, V), 0.0);

  // ambient
  vec3 La = vec3(0.2, 0.2, 0.2);    // ambient colour
  vec3 Ka = vec3(1.0, 1.0, 1.0);    // ambient light reflectance
  vec3 Ia = La * Ka;

  // diffuse
  vec3 Ld = vec3(0.7, 0.7, 0.7);    // diffuse light colour
  vec3 Kd = vec3(1.0, 0.5, 0.0);    // diffuse surface reflectance
  vec3 Id = Ld * Kd * dot_prod;

  // specular
  vec3 Ls = vec3(1.0, 1.0, 1.0);    // white specular colour
  vec3 Ks = vec3(1.0, 1.0, 1.0);    // fully reflect specular light
  float specular_exponent = 100.0;  // specular 'power'
  float specular_factor = pow(dot_prod_specular, specular_exponent);
  vec3 Is = Ls * Ks * specular_factor;

  
  vec4 light_colour  = vec4(Is + Id + Ia, 1.0);
  vec4 model_colour  = texture(texture_diffuse1, TexCoords);
  FragColor = light_colour * model_colour;
}
