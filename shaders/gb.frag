#version 150 core

out vec4 outColor;

void main( )
{ 
  outColor = vec4(1.0, 0.0, 0.0, 1.0);
  //outColor = vec4((TextureCoords.x - 0.5) / 160, (TextureCoords.y - 0.5) / 144, 0.0, 1.0);
}
