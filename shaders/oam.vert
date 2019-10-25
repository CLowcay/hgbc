#version 150 core

// An OBJ register
struct ObjRegister {
  ivec2 offset;
  int charCode;
  int attributes;
};

in ivec2 position;

out vec2 instanceOffset;
flat out int instanceCode;
flat out int instanceAttributes;

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

layout (std140) uniform OAMRegisters
{
  ObjRegister OBJ[40];
};

const int DisplayPriority = 0x80;
const int VerticalFlip = 0x40;
const int HorizontalFlip = 0x20;

const int LargeBlocks = 0x04;
const mat2 doubleHeight = mat2(1, 0,
                               0, 2);

const int BackgroundPriorityStart = 10241;

const ivec2 OAMOrigin = ivec2(-8, -16);

void main()
{ 
  instanceCode = OBJ[gl_InstanceID].charCode;

  instanceAttributes = OBJ[gl_InstanceID].attributes;
  bool isHorizontalFlip = bool(instanceAttributes & HorizontalFlip);
  bool isVerticalFlip = bool(instanceAttributes & VerticalFlip);
  bool isBgPriority = bool(instanceAttributes & DisplayPriority);

  ivec2 thisOffset = OBJ[gl_InstanceID].offset.yx;
  int zOffset = isBgPriority ? BackgroundPriorityStart : 0;

  vec2 realPosition;
  if((LCDC & LargeBlocks) != 0) {
    realPosition = (doubleHeight * position) + thisOffset + OAMOrigin;
    instanceOffset = vec2(isHorizontalFlip ? position.x : 7 - position.x,
                          isVerticalFlip ? 15 - position.y : position.y);
  } else {
    realPosition = position + thisOffset + OAMOrigin;
    instanceOffset = vec2(isHorizontalFlip ? position.x : 7 - position.x,
                          isVerticalFlip ? 7 - position.y : position.y);
  }

  gl_Position = projection * vec4(realPosition, (thisOffset.x * 40) + gl_InstanceID + zOffset, 1.0);
}