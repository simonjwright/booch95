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
