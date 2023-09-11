with PPrint; use PPrint;
with ExampleClass; use ExampleClass;
with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with ExampleTask; use ExampleTask;
with TestArray;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure Main is
   Example_String : String (1 .. 12);
   type My_Array is
      array (Integer range <>) of Integer;
   arr : constant My_Array := [1, 2, 3];
   arr2 : array (1 .. 3) of Integer := [1, 2, 3];
   type my_int is range 10 .. 12;
   type My_Array2 is
      array (my_int) of Integer;
   arr3 : constant My_Array2 := [1, 2, 3];

   arr4 : constant array (Character range <>) of Integer :=
   [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
   15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
    27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
    39, 40, 41, 42, 43, 44, 45, 46, 47, 47, 48, 49, 50];

   C : Class;
   C2 : Class;

   type Date is record
      Day : Integer range 1 .. 31;
      Month : Integer range 1 .. 12;
      Year : Integer range -4000 .. 4000 := 2023;
   end record;

   function Date_toString (D : Date) return String is
   begin
      return "Day: " & D.Day'Image & " Month: "
       & D.Month'Image & " Year: " & D.Year'Image;
   end Date_toString;

   Oliver_Birthday : Date := (7, 12, 1998);

   today : Date;
   Y : Integer renames today.Year;

   Next  : Time := Clock;

   package Integer_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Integer);

   use Integer_Vectors;

   V : Vector;

begin
   Example_String := "Hello World!";
   Print (Example_String);

   Print ("For loop with Index: ");
   for n in Index loop
      Print (Index'Image (n));
   end loop;

   Print ("For loop with array range and Integer range");
   for n in arr'Range loop
      Print (Integer'Image (n) & " " & Integer'Image (arr (n)));
   end loop;

   Print ("Fori loop ");
   for n in 1 .. 3 loop
      Print (Integer'Image (n) & " " & Integer'Image (arr2 (n)));
   end loop;

   Print ("Higher range: ");
   for n in arr3'Range loop
      Print (n'Image & " " & Integer'Image (arr3 (n)));
   end loop;

   Print ("Char Range: ");
   for n in arr4'Range loop
      if arr4 (n) > 40 then
         Print (n'Image & " " & Integer'Image (arr4 (n)));
      end if;
   end loop;

   Print ("Overloading and for-each");
   for n of arr3 loop
      Print (n'Image);
      Print (n);
   end loop;

   Print ("for-each and changing values");
   for n of arr2 loop
      n := 99;
      Print (n);
   end loop;

   Print ("fori and changing values");
   for n in 1 .. 3 loop
      arr2 (n) := n - 1;
      Print (n'Image & " " & Integer'Image (arr2 (n)));
   end loop;

   Print ("If, elsif and else");
   if 1 > 2 then
      Print ("?");
   elsif 1 = 2 then
      Print ("?");
   else
      Print ("!");
   end if;

   Print ("Class Example");
   C.Init;
   C2.Init;
   Print (C.toString);
   C.incrementA;
   Print (C.toString);
   incrementA (C, 99);
   Print (C.toString);
   Print (incrementB (C));
   Print (C.incrementB);

   Print ("C: " & C.toString);
   Print ("C2: " & C2.toString);

   Print ("record example");
   today.Day := 21;
   today.Month := 6;
   Print (Date_toString (Oliver_Birthday));

   Print (Date_toString (today));
   Y := 2024;
   Print (Date_toString (today));

   begin
      Oliver_Birthday.Day := 41;
      exception
         when E : Constraint_Error =>
            Print (Exception_Message (E));
   end;

   T.Continue;
   Next := Clock;
   Print ("Time at start: " & Time'Image (Next));
   Next := Next + Milliseconds (4000);

   T.Print;
   T.Blocking_Increment;
   T.Print;

   Print ("Time before delay: " & Time'Image (Clock));
   delay until Next;
   Print ("Time after delay: " & Time'Image (Clock));
   T.Blocking_Increment;
   Print ("Time last: " & Time'Image (Clock));
   T.Print;
   Print ("Time before Non-Block: " & Time'Image (Clock));
   T.Non_Blocking_Increment;
   Print ("Time after Non-Block: " & Time'Image (Clock));
   delay 2.0;
   T.Stop;

   TestArray.test;

   Print ("Containers");
   Print (V'Image);
   V.Append (2);
   Print (V'Image);

end Main;
