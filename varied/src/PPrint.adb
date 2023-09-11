with Ada.Text_IO; use Ada.Text_IO;
package body PPrint is
   procedure Print (text : String) is
   begin
      Put_Line (text);
   end Print;
   procedure Print (text : Integer) is
   begin
      Put_Line (text'Image);
   end Print;

begin
   null;
end PPrint;