#version 330 core
layout (location = 0) in vec3 aPos;

out vec3 colour;

uniform vec3 ball_colour;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main()
{
  colour = ball_colour;
  gl_Position = projection * view * model * vec4(aPos, 1.0f);
}

