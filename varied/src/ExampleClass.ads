package ExampleClass is
   type Class is tagged record
      A : Integer;
      B : Integer;
   end record;
   procedure Init (C : in out Class);
   function toString (C : in out Class) return String;
   procedure incrementA (C : in out Class; i : Integer := 1);
   function incrementB (C : in out Class; i : Integer := 1) return Integer;
private
end ExampleClass;