#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTexCoords;

out vec2 TexCoords;

out vec3 position_eye, normal_eye, N, light_position_world;
out mat4 view_matrix;
out vec3 vertex_world;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

uniform vec3 light_pos;

void main()
{
  mat4 model_view = view * model;
  mat3 normal_mat = mat3(model_view);
  
  position_eye = vec3(model_view * vec4(aPos, 1.0));
  normal_eye   = vec3(model_view  * vec4(aNormal, 0.0));
  N = normalize(normal_mat * aNormal);
  
  gl_Position  = projection * vec4(position_eye, 1.0);
  
  view_matrix = view;
  TexCoords = aTexCoords;
  
  light_position_world = light_pos;
  vertex_world = vec3(model * vec4(aPos, 1.0));
}
