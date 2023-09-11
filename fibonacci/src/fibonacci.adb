with Ada.Containers.Vectors;
with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
procedure Fibonacci is
   package Long_Long_Integer_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Long_Long_Integer);
   type Fib_Array is array (Natural range <>) of Long_Long_Integer;
   Fibonacci_Array : Fib_Array (0 .. 50);
   Fibonacci_Vector : Long_Long_Integer_Vectors.Vector;
   Start_Time : Time := Clock;
   function Fibonacci_Recursive (N : Integer) return Long_Long_Integer is
   begin
      if N = 0 then
         return 0;
      end if;
      if N = 1 then
         return 1;
      end if;
      return Fibonacci_Recursive (N - 1) + Fibonacci_Recursive (N - 2);
   end Fibonacci_Recursive;

begin
   --  Dynamic solution with a container
   Fibonacci_Vector.Append (0);
   Fibonacci_Vector.Append (1);
   for i in 2 .. 50 loop
      Fibonacci_Vector.Append (Fibonacci_Vector.Element (i - 1) + Fibonacci_Vector.Element (i - 2));
   end loop;
   Put_Line ("Dynamic solution with a container");
   Put_Line ("Answer:" & Long_Long_Integer'Image (Fibonacci_Vector (49)));
   Put_Line ("Time taken:" & Duration'Image (Clock - Start_Time));
   Start_Time := Clock;
   --  Dynamic solution with an array
   Fibonacci_Array (0) := 0;
   Fibonacci_Array (1) := 1;
   for i in 2 .. 50 loop
      Fibonacci_Array (i) := Fibonacci_Array (i - 1) + Fibonacci_Array (i - 2);
   end loop;
   Put_Line ("Dynamic solution with an array");
   Put_Line ("Answer:" & Long_Long_Integer'Image (Fibonacci_Array (49)));
   Put_Line ("Time taken:" & Duration'Image (Clock - Start_Time));
   Start_Time := Clock;
   --  Naive recursive solution
   Put_Line ("Naive recursive solution");
   Put_Line ("Answer:" & Long_Long_Integer'Image (Fibonacci_Recursive (49)));
   Put_Line ("Time taken:" & Duration'Image (Clock - Start_Time));
end Fibonacci;