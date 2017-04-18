class CreateUsers < ActiveRecord::Migration[5.0]
  def change
    create_table :users do |t|
      t.string :name
      t.float :position
      t.references :game

      t.timestamps
    end
  end
end
