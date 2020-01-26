#version 150 core

in vec2 position;
out vec2 textureCoord;

void main()
{ 
  textureCoord = (position.xy + vec2(1.0, 1.0)) / 2.0;
  gl_Position = vec4(position, 0.0, 1.0);
}