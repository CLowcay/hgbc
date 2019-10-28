#version 150 core

in vec2 instanceOffset;
flat in int instanceCode;
flat in int instanceAttributes;

out vec4 outColor;

uniform usamplerBuffer texCharacterData;

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

const int Palette = 0x10;

void main( )
{ 
  if (int(gl_FragCoord.y) != 143 - LY) {
    discard;
  }

  int palette = (instanceAttributes & Palette) == 0 ? OBP0 : OBP1;

  int ox = int(instanceOffset.x - 0.5);
  int oy = int(instanceOffset.y - 0.5);

  int b0 = int(texelFetch(texCharacterData, (instanceCode * 16) + (oy * 2)).r);
  int b1 = int(texelFetch(texCharacterData, (instanceCode * 16) + (oy * 2) + 1).r);
  int pixelIndex = ((b0 >> ox) & 1) | (((b1 >> ox) & 1) << 1);
  if (pixelIndex == 0) {
    discard;
  }

  int internalPaletteIndex = palette >> (pixelIndex * 2) & 3;
  float pixel = float(3 - internalPaletteIndex) / 3.0;
  outColor = vec4(pixel, pixel, pixel, 1);
}
