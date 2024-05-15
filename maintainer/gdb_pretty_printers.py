# This file gets embedded directly into the executables, via `#![debugger_visualizer()]` attributes.

import gdb
import re

ARRAY_VEC_REGEX = re.compile(r"^(arrayvec::(\w+::)+)ArrayVec<.+>$")
class ArrayVecPrinter:
    "Print an arrayvec::arrayvec::ArrayVec"

    def __init__(self, val):
        self.length = int(val["len"])
        self.array = val["xs"]

    def to_string(self):
        return f"ArrayVec(size={self.length})"

    def children(self):
        for i in range(self.length):
            # Deref the `MaybeUninit` and the `ManuallyDrop` inside.
            yield f"[{i}]",self.array[i]["value"]["value"]

    def num_children(self):
        return self.length

    @staticmethod
    def display_hint():
        return "array"

class ColorGroupPrinter(gdb.ValuePrinter):
    def __init__(self, val):
        array = val["colors"]
        self.length = int(array["len"])
        self.array = array["xs"]

    def to_string(self):
        return f"ColorSet(size={self.length})"

    def children(self):
        for i in range(self.length):
            # Deref the `MaybeUninit` and the `ManuallyDrop` inside.
            yield f"[{i}]",self.array[i]["value"]["value"]

    def num_children(self):
        return self.length

    @staticmethod
    def display_hint():
        return "array"

class Rgb16Printer(gdb.ValuePrinter):
    def __init__(self, val):
        raw = val["__0"]
        self.red = raw & 0x1F
        self.green = raw >> 5 & 0x1F
        self.blue = raw >> 10 & 0x1F
        self.alpha = raw >> 15

    def to_string(self):
        return f"Rgb16(r={self.red}, g={self.green}, b={self.blue}, a={self.alpha})"

class BitVecPrinter(gdb.ValuePrinter):
    def __init__(self, val, storage_type_name):
        storage_type = gdb.lookup_type(storage_type_name)
        span = val["bitspan"]
        self.length = span["len"]
        self.ptr = span["ptr"]["pointer"].cast(storage_type.pointer())
        self.storage_size = storage_type.sizeof

    def to_string(self):
        return f"BitVec(size={self.length})"

    def children(self):
        for i in range(self.length):
            bit_idx = i % self.storage_size
            storage = self.ptr[i // self.storage_size]

            # Only `Lsb0` is supported.
            yield f"[{i}]",(storage >> bit_idx) & 1

    def num_children(self):
        return self.length

    @staticmethod
    def display_hint():
        return "array"

def rgbds_lookup(val):
    lookup_tag = val.type.tag
    if lookup_tag is None:
        return None

    if ARRAY_VEC_REGEX.match(lookup_tag):
        return ArrayVecPrinter(val)
    if "rgbgfx::color_set::ColorSet" == lookup_tag:
        return ColorGroupPrinter(val)
    if "plumers::color::Rgb16" == lookup_tag:
        return Rgb16Printer(val)
    if "bitvec::vec::BitVec<usize, bitvec::order::Lsb0>" == lookup_tag:
        return BitVecPrinter(val, "usize")

    return None

gdb.current_objfile().pretty_printers.append(rgbds_lookup)
