#include "rectangle.h"

Rectangle::Rectangle() {
  width = 10;
  height = 10;
}

Rectangle::Rectangle(int a, int b) {
  width = a;
  height = b;
}

void Rectangle::set_values(int x, int y) {
  width = x;
  height = y;
}

int Rectangle::get_area() {
  return width * height;
}

int Rectangle::get_width() {
  return width;
}

int Rectangle::get_height() {
  return height;
}

void tell_rect_info(Rectangle *prect) {
  if (prect == NULL) {
    cout << "a null rectangle. :(" << endl;
    return;
  }

  cout << "Rectangle Area: " << prect->get_area() << endl;
  cout << "Rectangle Width: "<< prect->get_width() << endl;
  cout << "Rectangle Height: "<< prect->get_height() << endl;
}

int main() {
  Rectangle rect (10, 20);
  Rectangle rect1 (20, 40);
  Rectangle rect2 = Rectangle();
  tell_rect_info(&rect);
  tell_rect_info(&rect1);
  tell_rect_info(&rect2);
  Rectangle rect4 = {10, 5};
  tell_rect_info(&rect4);
  return 0;
}
