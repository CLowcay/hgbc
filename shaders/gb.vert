#version 150 core

in ivec2 position;
uniform mat4 projection;
uniform int line;

void main()
{ 
  gl_Position = projection * vec4(position.x, position.y + line, 0.0, 1.0);
}