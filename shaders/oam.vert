#version 150 core

in ivec2 position;
in ivec2 offset;
in int charCode;
in int attributes;
uniform mat4 projection;

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

void main()
{ 
  gl_Position = projection * vec4(position.x, position.y, 0.0, 1.0);
}