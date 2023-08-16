with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
--  Program which reverses the order of words in a string and prints the result
--  The String type in Ada is an array of characters and you can therefore use slices for manipulation.
--  Time Complexity: O(n)
procedure Reversewords is
   Before : constant String := "Pull up, Eva! Were here! Wave! Pull up!";
   After : String (1 .. Before'Length) := (others => ' ');
begin
   declare
      Last_Index : Natural := Before'Length;
      Next_Index : Natural := Last_Index;
      Length_Word : Positive := 1;
      After_Index : Positive := 1;
   begin
      loop
         Next_Index := Index (Before, " ", Last_Index, Backward);
         Length_Word := Last_Index - Next_Index - 1;
         if Next_Index = 0 then
            After (After_Index .. After_Index + Length_Word) := Before (Next_Index + 1 .. Last_Index);
            exit;
         end if;
         After (After_Index .. After_Index + Length_Word) := Before (Next_Index + 1 .. Last_Index);
         After_Index := After_Index + Length_Word + 2;
         Last_Index := Next_Index - 1;
      end loop;
   end;
   Put_Line (Before);
   Put_Line (After);
end Reversewords;