require 'current_user'

module ApplicationCable
  class Connection < ActionCable::Connection::Base
    identified_by :current_user

    include CurrentUser

    def connect
      self.current_user = find_verified_user
    end
  end
end
