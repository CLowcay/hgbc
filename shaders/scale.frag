#version 150 core

out vec4 outColor;
uniform sampler2D frame;
in vec2 textureCoord;

void main( )
{ 
  outColor = texture(frame, vec2(textureCoord.x, 1 - textureCoord.y));
}
