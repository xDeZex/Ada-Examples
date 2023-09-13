with Ada.Text_IO; use Ada.Text_IO;
package body Log is
   procedure Print (Do_Print : Boolean; Text : String) is
   begin
      if Do_Print then
         Put_Line (Text);
      end if;
   end Print;
end Log;