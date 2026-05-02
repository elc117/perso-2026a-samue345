serviceSpec :: Spec
serviceSpec = do
   describe "AppointmentService createAppointment" $ do
    it "cria um agendamento quando horário está livre" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      let appointment = Appointment
            { customerId = 1
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
            { customerId = 1
            , machine = 1
            , time = "10:00"
            }

      _ <- AppointmentService.createAppointment app appointment
      result <- AppointmentService.createAppointment app appointment

      result `shouldBe` Left "Essa máquina já está ocupada nesse horário."

  describe "AppointmentService listAppointments" $ do
    it "admin lista todos os agendamentos" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      let a1 = Appointment { customerId = 1, time = "10:00", machine = 1 }
      let a2 = Appointment { customerId = 2, time = "11:00", machine = 2 }

      AppointmentRepository.insertAppointment conn a1
      AppointmentRepository.insertAppointment conn a2

      result <- AppointmentService.listAppointments app True 0

      result `shouldBe` [a1, a2]

    it "usuário normal lista apenas seus agendamentos" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      let a1 = Appointment { customerId = 1, time = "10:00", machine = 1 }
      let a2 = Appointment { customerId = 2, time = "11:00", machine = 2 }

      AppointmentRepository.insertAppointment conn a1
      AppointmentRepository.insertAppointment conn a2

      result <- AppointmentService.listAppointments app False 1

      result `shouldBe` [a1]

  describe "AppointmentService listAppointments" $ do
    it "admin lista todos os agendamentos" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      let a1 = Appointment { customerId = 1, time = "10:00", machine = 1 }
      let a2 = Appointment { customerId = 2, time = "11:00", machine = 2 }

      AppointmentRepository.insertAppointment conn a1
      AppointmentRepository.insertAppointment conn a2

      result <- AppointmentService.listAppointments app True 0

      result `shouldBe` [a1, a2]

    it "usuário normal lista apenas seus agendamentos" $ do
      conn <- open ":memory:"
      createTables conn

      let app = App { appDb = conn }

      let a1 = Appointment { customerId = 1, time = "10:00", machine = 1 }
      let a2 = Appointment { customerId = 2, time = "11:00", machine = 2 }

      AppointmentRepository.insertAppointment conn a1
      AppointmentRepository.insertAppointment conn a2

      result <- AppointmentService.listAppointments app False 1

      result `shouldBe` [a1]

 describe "AppointmentService deleteAppointment" $ do
  it "admin pode deletar qualquer agendamento" $ do
    conn <- open ":memory:"
    createTables conn

    let app = App { appDb = conn }
    let appointment = Appointment { customerId = 1, time = "10:00", machine = 1 }

    AppointmentRepository.insertAppointment conn appointment

    result <- AppointmentService.deleteAppointment app True 999 1

    result `shouldBe` Right ()

  it "usuário pode deletar seu próprio agendamento" $ do
    conn <- open ":memory:"
    createTables conn

    let app = App { appDb = conn }
    let appointment = Appointment { customerId = 1, time = "10:00", machine = 1 }

    AppointmentRepository.insertAppointment conn appointment

    result <- AppointmentService.deleteAppointment app False 1 1

    result `shouldBe` Right ()

  it "usuário não pode deletar agendamento de outro usuário" $ do
    conn <- open ":memory:"
    createTables conn

    let app = App { appDb = conn }
    let appointment = Appointment { customerId = 1, time = "10:00", machine = 1 }

    AppointmentRepository.insertAppointment conn appointment

    result <- AppointmentService.deleteAppointment app False 2 1

    result `shouldBe` Left "Sem permissão para deletar"

  it "retorna erro quando agendamento não existe" $ do
    conn <- open ":memory:"
    createTables conn

    let app = App { appDb = conn }

    result <- AppointmentService.deleteAppointment app False 1 999

    result `shouldBe` Left "Agendamento não encontrado"