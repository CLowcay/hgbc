#version 150 core

out vec4 outColor;
uniform usamplerBuffer texCharacterData;
uniform usamplerBuffer texBackgroundData;
uniform int scx;
uniform int scy;
uniform int backgroundDataOffset;
uniform int characterDataOffset;
uniform int bgp;
in vec2 pixelPos;

void main( )
{ 
  int px = (int(pixelPos.x) + scx) & 255;
  int py = (int(pixelPos.y) + scy) & 255;
  int tx = px >> 3;
  int ty = py >> 3;
  int ox = 7 - (px & 7);
  int oy = py & 7;
  int tile = characterDataOffset +
      int(texelFetch(texBackgroundData, backgroundDataOffset + (ty * 32) + tx).r);

  int b0 = int(texelFetch(texCharacterData, (tile * 16) + (oy * 2)).r);
  int b1 = int(texelFetch(texCharacterData, (tile * 16) + (oy * 2) + 1).r);
  int pixelIndex = ((b0 >> ox) & 1) | (((b1 >> ox) & 1) << 1);
  int internalPaletteIndex = bgp >> (pixelIndex * 2) & 3;
  float pixel = float(3 - internalPaletteIndex) / 3.0;
  outColor = vec4(pixel, pixel, pixel, 1);
}
