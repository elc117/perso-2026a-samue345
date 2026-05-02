serviceSpec :: Spec
serviceSpec =
  describe "AppointmentService" $ do
    it "cria um agendamento quando horário está livre" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      let appointment = Appointment
            { customer = "Samuel"
            , machine = 1
            , time = "10:00"
            }

      result <- AppointmentService.createAppointment app appointment

      result `shouldBe` Right appointment

    it "bloqueia agendamento duplicado na mesma máquina e horário" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      let appointment = Appointment
            { customer = "Samuel"
            , machine = 1
            , time = "10:00"
            }

      _ <- AppointmentService.createAppointment app appointment
      result <- AppointmentService.createAppointment app appointment

      result `shouldBe` Left "Essa máquina já está ocupada nesse horário."