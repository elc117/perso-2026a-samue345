repositorySpec :: Spec
repositorySpec =
  describe "AppointmentRepository" $ do
    it "insere e encontra agendamento por máquina e horário" $ do
      conn <- open ":memory:"
      createTables conn

      let appointment = Appointment
            { customer_id = 1
            , machine = 1
            , time = "10:00"
            }

      AppointmentRepository.insertAppointment conn appointment

      exists <- AppointmentRepository.existsAppointmentAtMachineAndTime conn 1 "10:00"

      exists `shouldBe` True

    it "retorna False quando não existe agendamento" $ do
      conn <- open ":memory:"
      createTables conn

      exists <- AppointmentRepository.existsAppointmentAtMachineAndTime conn 1 "10:00"

      exists `shouldBe` False
      
    it "busca agendamento por id" $ do
  conn <- open ":memory:"
  createTables conn

  let appointment = Appointment { customerId = 1, time = "10:00", machine = 1 }

  AppointmentRepository.insertAppointment conn appointment

  result <- AppointmentRepository.findAppointmentById conn 1

  result `shouldBe` Just appointment

it "deleta agendamento por id" $ do
  conn <- open ":memory:"
  createTables conn

  let appointment = Appointment { customerId = 1, time = "10:00", machine = 1 }

  AppointmentRepository.insertAppointment conn appointment
  AppointmentRepository.deleteAppointmentById conn 1

  result <- AppointmentRepository.findAppointmentById conn 1

  result `shouldBe` Nothing