with PPrint; use PPrint;
package body TestArray is
    arr : unlimited_array := (1, 2, 3);
    procedure test is
    begin
        Print (arr'Image);
        Print (unlimited_array'Image (arr & arr));
        Print ("test");
    end test;
end TestArray;