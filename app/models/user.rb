class User < ApplicationRecord
  belongs_to :game, optional: true
end
