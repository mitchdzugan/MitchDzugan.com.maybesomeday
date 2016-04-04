class CreateAuthenticationTokenTable < ActiveRecord::Migration
  def change
    create_table :authentication_tokens, :id => false, :primary_key => :token do |t|
    	t.text :token
    	t.integer :user_id
    	t.datetime :expiration
    end
	add_index(:authentication_tokens, :token, :unique => true)
  end
end
