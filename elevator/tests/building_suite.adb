with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;
with Building.Tests;
package body Building_Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Test_Case_Access'(new Building.Tests.Elevator_Tests));
      return Ret;
   end Suite;

end Building_Suite;
