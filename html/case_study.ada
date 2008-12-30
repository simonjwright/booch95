--  $Id$

with Ada.Calendar;
with Ada.Strings.Bounded;
package Cars is

   package Plate_Strings
   is new Ada.Strings.Bounded.Generic_Bounded_Length (10);
   subtype Plate_String
   is Plate_Strings.Bounded_String;

   package Model_Strings
   is new Ada.Strings.Bounded.Generic_Bounded_Length (32);
   subtype Model_String
   is Model_Strings.Bounded_String;

   type Car is record
      Plate : Plate_String;
      Model : Model_String;
      Registered : Ada.Calendar.Time;
   end record;

end Cars;

with BC.Containers;
with Cars;
package Abstract_Car_Containers
is new BC.Containers (Cars.Car, "=" => Cars."=");

with Abstract_Car_Containers;
with BC.Containers.Collections;
package Abstract_Car_Collections
is new Abstract_Car_Containers.Collections;

with Abstract_Car_Collections;
with BC.Containers.Collections.Bounded;
package Fleets
is new Abstract_Car_Collections.Bounded (Maximum_Size => 30);

with Fleets;
package My_Fleet is

   The_Fleet : Fleets.Collection;

end My_Fleet;

with BC.Containers.Collections.Bounded;
with Cars;
package My_Fleet_Combined is

   use type Cars.Car;

   package Abstract_Car_Containers
   is new BC.Containers (Cars.Car);

   package Abstract_Car_Collections
   is new Abstract_Car_Containers.Collections;

   package Fleets
   is new Abstract_Car_Collections.Bounded (Maximum_Size => 30);

   The_Fleet : Fleets.Collection;

end My_Fleet_Combined;

with BC.Containers.Collections.Bounded;
with Cars;
package My_Fleet_Hidden is

   --  subprograms to add, find, and delete Cars

private

   package Abstract_Car_Containers
   is new BC.Containers (Cars.Car, "=" => Cars."=");

   package Abstract_Car_Collections
   is new Abstract_Car_Containers.Collections;

   package Fleets
   is new Abstract_Car_Collections.Bounded (Maximum_Size => 30);

   The_Fleet : Fleets.Collection;

end My_Fleet_Hidden;

with Abstract_Car_Containers;
with Cars; use Cars;
with Fleets;
with My_Fleet; use My_Fleet;
procedure Iterate is
begin

   declare

      It : Abstract_Car_Containers.Iterator'Class
        := Fleets.New_Iterator (The_Fleet);

   begin

      while not Abstract_Car_Containers.Is_Done (It) loop

         declare
            C : Car := Abstract_Car_Containers.Current_Item (It);
         begin
            null;  --  do something with C
         end;

         Abstract_Car_Containers.Next (It);

      end loop;

   end;

   declare

      procedure Process_Car (C : Car; OK : out Boolean);

      procedure Process_Fleet
        is new Abstract_Car_Containers.Visit (Process_Car);

      procedure Process_Car (C : Car; OK : out Boolean) is
      begin

         OK := True;  --  unless you want the iteration to stop early

         --  do something with C

      end Process_Car;

      It : Abstract_Car_Containers.Iterator'Class
        := Fleets.New_Iterator (The_Fleet);

   begin

      Process_Fleet (It);

   end;
end Iterate;

with Abstract_Car_Containers;
with Ada.Calendar;
with BC.Filter;
with Cars; use Cars;
with Fleets;
with My_Fleet; use My_Fleet;
procedure Filter_Demo is

   function Registered_Before
     (Date : Ada.Calendar.Time) return Fleets.Collection is

      function Wanted (C : Car) return Boolean;

      procedure Choose is new BC.Filter
        (Item => Car,
         Source => Abstract_Car_Containers,
         From => Fleets.Collection,
         Target => Abstract_Car_Containers,
         To => Fleets.Collection,
         Pass => Wanted,
         Clear => Fleets.Clear,
         Add => Fleets.Append);

      function Wanted (C : Car) return Boolean is
         use type Ada.Calendar.Time;
      begin
         return C.Registered < Date;
      end Wanted;

      Result : Fleets.Collection;

   begin
      Choose (The_Fleet, Result);
      return Result;
   end Registered_Before;

begin
   null;
end Filter_Demo;

with Abstract_Car_Containers;
with Ada.Calendar;
with BC.Containers.Quicksort;
with Cars; use Cars;
with Fleets;
with My_Fleet; use My_Fleet;
procedure Sort_Demo is

   procedure Sort is

      function Earlier (L, R : Car) return Boolean;

      procedure Sort is new Abstract_Car_Containers.Quicksort
        ("<" => Earlier,
         Container => Fleets.Collection,
         Length => Fleets.Length);

      function Earlier (L, R : Car) return Boolean is
         use type Ada.Calendar.Time;
      begin
         return L.Registered < R.Registered;
      end Earlier;

   begin
      Sort (The_Fleet);
   end Sort;

begin
   null;
end Sort_Demo;

with Abstract_Car_Containers;
with BC.Containers.Maps;
with Cars;
package Abstract_Car_Maps
is new Abstract_Car_Containers.Maps (Key => Cars.Plate_String,
                                       "=" => Cars.Plate_Strings."=");

with Cars;
function Plate_Hash (P : Cars.Plate_String) return Natural;

with Abstract_Car_Maps;
with BC.Containers.Maps.Bounded;
with Plate_Hash;
package Mapped_Fleets is new Abstract_Car_Maps.Bounded
  (Hash => Plate_Hash,
   Buckets => 5,
   Maximum_Size => 30);


