#include <iostream>

using namespace std;

class Rectangle {
  int width, height;
public:
  Rectangle();
  Rectangle(int, int);
  void set_values(int, int);
  int get_area();
  int get_width();
  int get_height();
};

