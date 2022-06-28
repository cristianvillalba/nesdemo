# nes dev demo
Raymarching a Mate (Distance Field) in NES.

This is some experiment that I did trying to learn 6502 ASM development.

Raymarching a cut hollow sphere, with a pill on it.
This will somehow asemble a kind of Mate from Argentina.

I render all the pixels directly to the PPU pattern table, so no predraw CHR files here.
Also it is using the sprite 0 hit for switching the table and paint almost 500 sprites in screen.

It is really slow, so you better use the turbo option in your emulator to get decent speed.

ToDos:
- [X] making a Mate with DFs.
- [ ] putting some sound on it.
- [ ] stop wasting time here.

![image](https://user-images.githubusercontent.com/5841150/176202903-83dee980-5954-48ca-89ee-5a8921b7d429.png)



