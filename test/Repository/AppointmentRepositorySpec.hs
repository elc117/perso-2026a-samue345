repositorySpec :: Spec
repositorySpec =
  describe "AppointmentRepository" $ do
    it "insere e encontra agendamento por máquina e horário" $ do
      conn <- open ":memory:"
      createTables conn

      let appointment = Appointment
            { customer = "Samuel"
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