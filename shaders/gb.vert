#version 150 core

in ivec2 position;
uniform mat4 projection;
uniform int line;
out vec2 pixelPos;

void main()
{ 
  pixelPos = vec2(position.x, line);
  gl_Position = projection * vec4(position.x, position.y + line, 0.0, 1.0);
}