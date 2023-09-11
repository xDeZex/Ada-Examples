with PPrint; use PPrint;
with Ada.Real_Time; use Ada.Real_Time;
package body ExampleTask is

   task body T is
      count : Integer := 0;

   begin
      Print ("Started Task");
      select
         accept Continue do
               Print ("Continued the task");
         end Continue;
         loop
               select
                  accept Blocking_Increment do
                     Print ("Start of Increment");
                     delay 2.0;
                     count := count + 1;
                     Print ("End of Increment");
                  end Blocking_Increment;
               or
                  accept Non_Blocking_Increment do
                     null;
                  end Non_Blocking_Increment;
                  Print ("Start of Increment");
                  delay 1.0;
                  count := count + 1;
                  Print ("End of Increment");
                  Print ("Time after Non-Block is done: " & Time'Image (Clock));
               or
                  accept Print do
                     Print ("Task at: " & count'Image);
                  end Print;
               or
                  accept Stop do
                     Print ("Stopped the task");
                  end Stop;
                  exit;
               end select;
         end loop;
      or
         terminate;
      end select;
   end T;
end ExampleTask;