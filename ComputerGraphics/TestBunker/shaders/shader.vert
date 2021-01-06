#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTexCoords;

out vec2 texture_coordinate;
out vec3 vertex_world;
out vec3 tnorm;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
  vertex_world = vec3(model * vec4(aPos, 1.0));
  tnorm = normalize(vec3(model * vec4(aNormal, 0.0)));
  texture_coordinate = aTexCoords;
  gl_Position = projection * view * model * vec4(aPos, 1.0);
}


