# Computer Graphics Lab

![standard-readme compliant](https://img.shields.io/github/license/SugarSBN/PL0-Compiler)![](https://img.shields.io/badge/language-Haskell-brightgreen)![](https://img.shields.io/badge/base-^>=4.14.3.0-brightgreen)

A toy interactive graphic software for 3D world free tour and smooth transforming animation, implemented with Haskell/OpenGL/GLFW.

- [Feature](#feature)
- [Usage](#usage)
- [License](#license)

## Feature

1. 从.obj或.ply文件读取一个三维模型，能够使用鼠标对模型进行平移、放缩和旋转操作。其中旋转操作要方便、直观、符合三维操作习惯。随着课程的进行，支持正投影和透视投影两种观察方式切换，支持基于光照的真实感和预定义物体颜色的两种绘制方式。

2. 能够对模型进行instance操作，将模型通过变换形成多个实例。
3. 基于上述功能，顺序定义同一3D物体的四个不同实例（具有不同位置和姿态），编程实现此物体沿着由四个不同实例确定的三次Bezier曲线移动，同时物体姿态由初始姿态（第一个实例的姿态）到最终姿态（第四个物体的姿态）进行光滑旋转。可控制物体整个运动过程所需时间长短。
4. 编程实现从物体多个实例中用鼠标选中其中一个物体，然后对其进行各种变换操作。

## Usage

```bash
cd Lab
cabal run
```

Use `W,A,S,D` to tour freely.

Use the left button of your mouse to select and copy models.

Use the right button of your mouse to unselect and remove models.

Select a model and use `↑↓←→` to rotate it freely.

Once you have four or more instances, press `Space` and enjoy the smooth animation, use `Q` and `E` to speed up or down the animation.

## License

[MIT © Richard McRichface.](../LICENSE)