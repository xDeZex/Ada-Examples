with PPrint; use PPrint;
package body ExampleClass is
   procedure Init (C : in out Class) is
   begin
      C.A := 1;
      C.B := 2;
   end Init;
   function toString (C : in out Class) return String is
   begin
      return "A is: " & C.A'Image & " B is: " & C.B'Image;
   end toString;
   procedure incrementA (C : in out Class; i : Integer := 1) is
   begin
      Print ("Incrementing A");
      C.A := C.A + i;
   end incrementA;

   function incrementB (C : in out Class; i : Integer := 1) return Integer is
   begin
      Print ("Incrementing B");
      C.B := C.B + i;
      return C.B;
   end incrementB;
end ExampleClass;