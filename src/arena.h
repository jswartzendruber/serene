#ifndef ARENA_H
#define ARENA_H

template <typename T>
class Arena {
 public:
  Arena(int size) {
    data = new T[size];
    len = 0;
  }

  ~Arena() { delete[] data; }

  void put(T elem) { data[len++] = elem; }

  T get(int idx) { return data[idx]; }

 private:
  T *data;
  int len;
};

#endif