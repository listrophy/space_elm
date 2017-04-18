class User < ApplicationRecord
  has_one :player
  has_one :game, through: :player
end
