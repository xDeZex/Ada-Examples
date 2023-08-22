with AUnit.Reporter.Text;
with AUnit.Run;
with Elevator_Suite; use Elevator_Suite;

procedure Harness is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Harness;