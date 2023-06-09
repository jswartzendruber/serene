#ifndef VEC_H
#define VEC_h

template <typename T>
class Vec {
 public:
  Arena(int size) {
    data = new T[size];
    size = 0;
    len = 0;
  }

  ~Arena() { delete[] data; }

  void put(T elem) { data[len++] = elem; }

  T get(int idx) { return data[idx]; }

 private:
  T *data;
  int len;
  int size;
};

#endif