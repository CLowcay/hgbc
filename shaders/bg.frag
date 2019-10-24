#version 150 core

out vec4 outColor;
uniform usamplerBuffer texCharacterData;
uniform usamplerBuffer texBackgroundData;
in vec2 pixelPos;

layout (std140) uniform Registers
{
  int LCDC; // 0xFF40
  int STAT; // 0xFF41
  int SCY;  // 0xFF42
  int SCX;  // 0xFF43
  int LY;   // 0xFF44
  int LYC;  // 0xFF45
  int DMA;  // 0xFF46
  int BGP;  // 0xFF47
  int OBP0; // 0xFF48
  int OBP1; // 0xFF49
  int WY;   // 0xFF4A
  int WX;   // 0xFF4B
};

const float BackgroundFrontLayer = 10240/20481;

void main( )
{ 
  int characterDataOffset = (LCDC & 0x10) == 0 ? 0x800 : 0;
  int codeAreaOffset = (LCDC & 0x08) == 0 ? 0 : 0x400;

  int px = (int(pixelPos.x) + SCX) & 255;
  int py = (int(pixelPos.y) + SCY) & 255;
  int tx = px >> 3;
  int ty = py >> 3;
  int ox = 7 - (px & 7);
  int oy = py & 7;
  int rawTile = int(texelFetch(texBackgroundData, codeAreaOffset + (ty * 32) + tx).r);
  int tile = rawTile > 127 ? rawTile : characterDataOffset + rawTile;

  int b0 = int(texelFetch(texCharacterData, (tile * 16) + (oy * 2)).r);
  int b1 = int(texelFetch(texCharacterData, (tile * 16) + (oy * 2) + 1).r);
  int pixelIndex = ((b0 >> ox) & 1) | (((b1 >> ox) & 1) << 1);
  int internalPaletteIndex = BGP >> (pixelIndex * 2) & 3;
  float pixel = float(3 - internalPaletteIndex) / 3.0;

  if (pixelIndex != 0) {
    gl_FragDepth = BackgroundFrontLayer;
  }

  outColor = vec4(pixel, pixel, pixel, 1);
}
