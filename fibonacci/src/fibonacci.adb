with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
procedure Fibonacci is
   package Long_Long_Integer_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Long_Long_Integer);
   Fib_Vector : Long_Long_Integer_Vectors.Vector;
begin
   Fib_Vector.Append (0);
   Fib_Vector.Append (1);
   for i in 2 .. 50 loop
      Fib_Vector.Append (Fib_Vector.Element (i - 1) + Fib_Vector.Element (i - 2));
   end loop;
   Put_Line (Long_Long_Integer'Image (Fib_Vector (49)));
end Fibonacci;