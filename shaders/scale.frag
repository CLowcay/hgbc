#version 150 core

out vec4 outColor;
uniform sampler2D frame;
in vec2 textureCoord;

void main( )
{ 
  vec4 color = texture(frame, vec2(textureCoord.x, 1 - textureCoord.y));
  outColor = vec4(color.r, color.g, color.b, 1.0);
}
